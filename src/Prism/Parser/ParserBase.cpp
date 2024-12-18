#include "Prism/Parser/ParserBase.h"

using namespace prism;
using ranges::views::enumerate;

void ParserBase::parseLinearGrammarImpl(std::span<ParserRule const> rules,
                                        std::span<Facet const*> facets,
                                        RecoveryOptions recoveryOptions) {
    recovOptStack.push(recoveryOptions);
    utl::scope_guard pop = [this] { recovOptStack.pop(); };
    PRISM_ASSERT(rules.size() == facets.size());
    auto inc = [&](size_t offset) {
        rules = rules.subspan(offset);
        facets = facets.subspan(offset);
    };
    uint32_t startTokenIndex = tokenIndex;
    bool first = true;
    while (!rules.empty()) {
        auto& rule = rules.front();
        auto& facet = facets.front();
        facet = rule.parser();
        if (facet) {
            inc(1);
            first = false;
            continue;
        }
        // If the first rule fails we return directly
        if (first) {
            PRISM_ASSERT(
                startTokenIndex == tokenIndex,
                "We should fail gracefully here without eating tokens");
            return;
        }
        // Emit the rule error if we are not recovering
        if (!inRecovery && rule.error) rule.error(peek());
        // We are committed, so we try to recover
        if (auto offset = recoverLinearGrammar(rules, facets, recoveryOptions))
            inc(*offset);
        else
            return;
    }
}

std::optional<size_t> ParserBase::recoverLinearGrammar(
    std::span<ParserRule const> rules, std::span<Facet const*> facets,
    RecoveryOptions recoveryOptions) {
    PRISM_ASSERT(!rules.empty());
    PRISM_ASSERT(rules.size() == facets.size());
    if (!rules.front().backtrackIfFailed) {
        return recoverLinearGrammarImpl(rules, facets, recoveryOptions);
    }
    pushBacktrackingAnchor();
    auto result = recoverLinearGrammarImpl(rules, facets, recoveryOptions);
    if (result) {
        popBacktrackingAnchor();
        return result;
    }
    else {
        performBacktrack();
        return std::nullopt;
    }
}

template <typename T, std::convertible_to<T> U>
static auto exchangeForScope(T& state, U&& tmp) {
    auto guard = [&state, stashed = state] { state = stashed; };
    state = std::forward<U>(tmp);
    return utl::scope_guard(guard);
}

std::optional<size_t> ParserBase::recoverLinearGrammarImpl(
    std::span<ParserRule const> rules, std::span<Facet const*> facets,
    RecoveryOptions recoveryOptions) {
    auto guard = exchangeForScope(inRecovery, true);
    // We attempt to recover from the error by parsing subsequent rules in the
    // grammer. If we succeed, we leave the missing rule as null and continue
    // where we succeeded. Otherwise we eat a token and try again a couple of
    // times.
    for (int i = 0; i < recoveryOptions.numAttempts; ++i) {
        if (i > 0) {
            if (recoveryOptions.isStop(peek())) return std::nullopt;
            raise<UnexpectedToken>(peek());
            eat();
        }
        auto [facet, recoveredIndex] =
            findFacetForRecovery(rules,
                                 /* isFirst: */ i == 0);
        if (facet) {
            facets[recoveredIndex] = facet;
            return recoveredIndex + 1;
        }
    }
    return std::nullopt;
}

std::pair<Facet const*, size_t> ParserBase::findFacetForRecovery(
    std::span<ParserRule const> rules, bool isFirst) {
    for (auto [index, rule]: rules | enumerate) {
        // We continue here because we already tried to parse the first rule
        // and it failed
        if (isFirst && index == 0) continue;
        if (auto* facet = rule.parser()) return { facet, index };
    }
    return {};
}

static auto tokEqFn(TokenKind kind) {
    return [kind](TokenKind k) { return k == kind; };
}

static auto tokEqFn(std::span<TokenKind const> kinds) {
    return [kinds](TokenKind k) { return ranges::contains(kinds, k); };
}

std::optional<Token> ParserBase::match(TokenKind kind) {
    return matchImpl(true, tokEqFn(kind));
}

std::optional<Token> ParserBase::match(VolatileList<TokenKind const> kinds) {
    return matchImpl(true, tokEqFn(kinds));
}

std::optional<Token> ParserBase::peekMatch(TokenKind kind) {
    return matchImpl(false, tokEqFn(kind));
}

std::optional<Token> ParserBase::matchImpl(bool eat, auto verify) {
    auto tok = peek();
    if (!verify(tok.kind)) return std::nullopt;
    if (eat) this->eat();
    return tok;
}

std::optional<Token> ParserBase::peekMatch(
    VolatileList<TokenKind const> kinds) {
    return matchImpl(false, tokEqFn(kinds));
}

Token ParserBase::peek() { return eatPeekImpl(0); }

Token ParserBase::eat() { return eatPeekImpl(1); }

Token ParserBase::eatPeekImpl(uint32_t offset) {
    if (tokenIndex < tokens.size()) {
        auto tok = tokens[tokenIndex];
        tokenIndex += offset;
        return tok;
    }
    auto tok = lexer.next();
    tokens.push_back(tok);
    tokenIndex += offset;
    return tok;
}
