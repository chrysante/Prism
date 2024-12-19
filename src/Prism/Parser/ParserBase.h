#ifndef PRISM_PARSER_PARSERBASE_H
#define PRISM_PARSER_PARSERBASE_H

#include <array>
#include <concepts>
#include <cstdint>
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

/// Generic parsing functionality that is independent of the language grammar
struct ParserBase {
    explicit ParserBase(MonotonicBufferResource& alloc,
                        SourceContext const& sourceCtx, IssueHandler& iss):
        alloc(alloc),
        sourceCtx(sourceCtx),
        lexer(sourceCtx.source(), iss),
        iss(iss) {}

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
    auto Match(std::same_as<TokenKind> auto... kinds) {
        return [... kinds = kinds, this]() -> TerminalFacet const* {
            if (auto tok = match({ kinds... }))
                return allocate<TerminalFacet>(*tok);
            return nullptr;
        };
    }

    /// \overload
    auto Match(std::span<TokenKind const> kinds) {
        return [this, kinds]() -> TerminalFacet const* {
            if (auto tok = match(kinds)) return allocate<TerminalFacet>(*tok);
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
    auto Raise(Args&&... args) {
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

protected:
    uint32_t currentTokenIndex() const { return tokenIndex; }

private:
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
};

} // namespace prism

#endif // PRISM_PARSER_PARSERBASE_H
