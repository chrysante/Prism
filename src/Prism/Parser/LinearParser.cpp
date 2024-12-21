#include "Prism/Parser/LinearParser.h"

using ranges::views::enumerate;

using namespace prism;

void LinearParser::parseLinearGrammarImpl(
    std::span<ParserRule const> rules, std::span<Facet const*> facets,
    RecoveryOptions const& recoveryOptions) {
    recovOptStack.push(recoveryOptions);
    utl::scope_guard pop = [this] { recovOptStack.pop(); };
    PRISM_ASSERT(rules.size() == facets.size());
    auto inc = [&](size_t offset) {
        rules = rules.subspan(offset);
        facets = facets.subspan(offset);
    };
    while (!rules.empty()) {
        uint32_t startTokenIndex = currentTokenIndex();
        auto& rule = rules.front();
        auto& facet = facets.front();
        facet = rule.parser();
        if (facet) {
            inc(1);
            continue;
        }
        // Okay, we failed to parse the current fule.
        // For fast fail rules we return directly
        if (rule.isFastFail) {
            PRISM_ASSERT(
                startTokenIndex == currentTokenIndex(),
                "We should fail gracefully here without eating tokens");
            return;
        }
        // We are committed, so we try to recover
        if (auto offset = recoverLinearGrammar(rules, facets, recoveryOptions))
            inc(*offset);
        else
            return;
    }
}

std::optional<size_t> LinearParser::recoverLinearGrammar(
    std::span<ParserRule const> rules, std::span<Facet const*> facets,
    RecoveryOptions const& recoveryOptions) {
    PRISM_ASSERT(!rules.empty());
    PRISM_ASSERT(rules.size() == facets.size());
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

static void emitErrors(std::span<ParserRule const> rules, Token const& tok) {
    for (auto& missed: rules)
        if (missed.error) missed.error(tok);
}

std::optional<size_t> LinearParser::recoverLinearGrammarImpl(
    std::span<ParserRule const> rules, std::span<Facet const*> facets,
    RecoveryOptions const& recoveryOptions) {
    auto guard = exchangeForScope(inRecovery, true);
    // We attempt to recover from the error by parsing subsequent rules in the
    // grammer. If we succeed, we leave the missing rule as null and continue
    // where we succeeded. Otherwise we eat a token and try again a couple of
    // times.
    auto tok = peek();
    for (int i = 0; i < recoveryOptions.numAttempts; ++i) {
        if (i > 0) {
            if (recoveryOptions.isStop(peek())) break;
            raise<UnexpectedToken>(peek());
            eat();
        }
        auto [facet, recoveredIndex] =
            findFacetForRecovery(rules,
                                 /* isFirst: */ i == 0);
        if (facet) {
            facets[recoveredIndex] = facet;
            emitErrors(rules.subspan(0, recoveredIndex), tok);
            return recoveredIndex + 1;
        }
    }
    emitErrors(rules, tok);
    return std::nullopt;
}

std::pair<Facet const*, size_t> LinearParser::findFacetForRecovery(
    std::span<ParserRule const> rules, bool isFirst) {
    for (auto [index, rule]: rules | enumerate) {
        // We continue here because we already tried to parse the first rule
        // and it failed
        if (isFirst && index == 0) continue;
        auto* facet = rule.parser();
        if (!facet) continue;
        return { facet, index };
    }
    return {};
}
