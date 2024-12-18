#ifndef PRISM_PARSER_PARSERBASE_H
#define PRISM_PARSER_PARSERBASE_H

#include <array>
#include <concepts>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <vector>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/function_view.hpp>
#include <utl/scope_guard.hpp>
#include <utl/stack.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Lexer/Lexer.h"
#include "Prism/Parser/SyntaxIssue.h"
#include "Prism/Source/SourceContext.h"

namespace prism {

template <typename T>
struct VolatileList: std::span<T> {
    template <typename... Args>
        requires std::constructible_from<std::span<T>, Args&&...>
    VolatileList(Args&&... args): std::span<T>(std::forward<Args>(args)...) {}
    VolatileList(std::span<T> s): std::span<T>(s) {}
    VolatileList(std::initializer_list<std::remove_const_t<T>> ilist):
        std::span<T>(ilist) {}
    VolatileList(T& kind): std::span<T>(&kind, 1) {}
};

struct ParserRuleOptions {
    bool backtrackIfFailed = false;
};

template <typename F>
concept ParserRuleFn =
    std::convertible_to<std::invoke_result_t<F>, Facet const*>;

struct ParserRule: ParserRuleOptions {
    ParserRule(ParserRuleFn auto&& parser,
               std::function<void(Token)> error = {},
               ParserRuleOptions opt = {}):
        ParserRuleOptions(opt),
        parser(std::move(parser)),
        error(std::move(error)) {}

    std::function<Facet const*()> parser;
    std::function<void(Token)> error;
};

struct RecoveryOptions {
    std::function<bool(Token)> isStop;
    int numAttempts = 3;
};

template <size_t N>
std::array<Facet const*, N> unpack(Facet const* facet) {
    if (!facet) return {};
    std::array<Facet const*, N> res;
    auto* list = cast<ListFacet const*>(facet);
    PRISM_ASSERT(list->children().size() == N);
    ranges::copy(list->children(), res.begin());
    return res;
}

/// Generic parsing functionality that is independent of the language grammar
class ParserBase {
public:
    explicit ParserBase(MonotonicBufferResource& alloc,
                        SourceContext const& sourceCtx, IssueHandler& iss):
        alloc(alloc),
        sourceCtx(sourceCtx),
        lexer(sourceCtx.source(), iss),
        iss(iss) {}

protected:
    template <size_t N>
        requires(N > 0)
    std::array<Facet const*, N> parseLinearGrammar(
        ParserRule const (&rules)[N], RecoveryOptions recoveryOptions) {
        std::array<Facet const*, N> facets{};
        parseLinearGrammarImpl(rules, facets, recoveryOptions);
        return facets;
    }

    template <size_t N>
        requires(N > 0)
    ParserRule option(ParserRule const (&rules)[N]) {
        return { [&]() -> ListFacet const* {
            auto elems = parseLinearGrammarNoRecov(rules);
            if (elems.front()) return allocate<ListFacet>(elems);
            return nullptr;
        }, {}, { .backtrackIfFailed = true } };
    }

    // MARK: Facet allocation
    template <typename T, typename... Args>
        requires std::constructible_from<T, Args&&...>
    T* allocate(Args&&... args) {
        return prism::allocate<T>(alloc, std::forward<Args>(args)...);
    }

    template <typename T, typename... Args>
        requires std::constructible_from<T, MonotonicBufferResource&, Args&&...>
    T* allocate(Args&&... args) {
        return prism::allocate<T>(alloc, alloc, std::forward<Args>(args)...);
    }

    // MARK: Token consumption
    /// _Eats_ and returns the eaten token, if its kind is any of the given
    /// arguments. Otherwise returns nullopt
    std::optional<Token> match(TokenKind tok);

    /// \overload
    std::optional<Token> match(VolatileList<TokenKind const> tokenKinds);

    /// \Returns a function calling `match(kinds)`
    auto matcher(std::same_as<TokenKind> auto... kinds) {
        return [... kinds = kinds, this]() -> TerminalFacet const* {
            if (auto tok = match({ kinds... }))
                return allocate<TerminalFacet>(*tok);
            return nullptr;
        };
    }

    /// _Peeks_ and returns the peeked token, if its kind is any of the given
    /// arguments. Otherwise returns nullopt
    std::optional<Token> peekMatch(TokenKind tok);

    /// \overload
    std::optional<Token> peekMatch(VolatileList<TokenKind const> tokenKinds);

    /// \Returns the next token in the stream without consuming it
    Token peek();

    /// \Returns the next token in the stream and consumes it
    Token eat();

    // MARK: Issue
    ///
    template <std::derived_from<Issue> I, typename... Args>
        requires std::constructible_from<I, Args&&...>
    void raise(Args&&... args) {
        iss.push<I>(std::forward<Args>(args)...);
    }

    template <std::derived_from<Issue> I, typename... Args>
        requires std::constructible_from<I, Token, Args&&...>
    auto raiser(Args&&... args) {
        return [... args = std::forward<Args>(args), this](Token token) {
            raise<I>(token, args...);
        };
    }

    // MARK: Backtracking
    void pushBacktrackingAnchor() { backtrackingAnchorStack.push(tokenIndex); }

    void popBacktrackingAnchor() {
        PRISM_ASSERT(!backtrackingAnchorStack.empty());
        backtrackingAnchorStack.pop();
    }

    void performBacktrack() {
        PRISM_ASSERT(!backtrackingAnchorStack.empty());
        tokenIndex = backtrackingAnchorStack.pop();
    }

private:
    // MARK: Automatic error recovery
    template <size_t N>
    std::array<Facet const*, N> parseLinearGrammarNoRecov(
        ParserRule const (&rules)[N]) {
        return parseLinearGrammar(rules, recovOptStack.top());
    }

    void parseLinearGrammarImpl(std::span<ParserRule const> rules,
                                std::span<Facet const*> facets,
                                RecoveryOptions recoveryOptions);

    std::optional<size_t> recoverLinearGrammar(
        std::span<ParserRule const> rules, std::span<Facet const*> facets,
        RecoveryOptions recoveryOptions);

    std::optional<size_t> recoverLinearGrammarImpl(
        std::span<ParserRule const> rules, std::span<Facet const*> facets,
        RecoveryOptions recoveryOptions);

    std::pair<Facet const*, size_t> findFacetForRecovery(
        std::span<ParserRule const> rules, bool first);

    // MARK: Other implementation details

    /// Implementation of `match()` and `peekMatch()`
    std::optional<Token> matchImpl(bool eat, auto verify);

    /// Implementation of `eat()` and `peek()`
    Token eatPeekImpl(uint32_t offset);

    MonotonicBufferResource& alloc;
    SourceContext const& sourceCtx;
    IssueHandler& iss;
    Lexer lexer;
    std::vector<Token> tokens;
    uint32_t tokenIndex = 0;
    utl::stack<uint32_t> backtrackingAnchorStack;
    utl::stack<RecoveryOptions> recovOptStack;
    bool inRecovery = false;
};

} // namespace prism

#endif // PRISM_PARSER_PARSERBASE_H
