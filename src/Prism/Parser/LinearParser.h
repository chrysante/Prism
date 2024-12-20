#include "Prism/Parser/ParserBase.h"

#include <functional>

namespace prism {

struct ParserRuleOptions {};

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
    bool isFastFail = false;

private:
    friend struct LinearParser;
    ParserRule() = default;
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

template <size_t I, typename... Children>
struct IndexTree {
    static constexpr size_t Index = I;
};

template <typename... Forest>
constexpr size_t computeUnpackedSize(size_t N) {
    auto f = [&]<size_t I, typename... Children>(auto& f,
                                                 IndexTree<I, Children...>) {
        size_t numChildren = sizeof...(Children);
        if (numChildren > 0) N += numChildren - 1;
        (f(f, Children{}), ...);
    };
    (f(f, Forest{}), ...);
    return N;
}

template <typename... Forest>
void unpackResult(std::span<Facet const* const> packed,
                  std::span<Facet const*> result) {

    auto f = [&]<size_t I, typename... Children>(auto& f, size_t& offset,
                                                 std::span<Facet const* const>
                                                     packed,
                                                 std::span<Facet const*> result,
                                                 IndexTree<I, Children...>) {
        static constexpr size_t NumChildren = sizeof...(Children);
        if constexpr (NumChildren == 0) {
            result[I + offset] = packed[I];
        }
        else {
            auto childNodes = unpack<NumChildren>(packed[I]);
            size_t subOffset = 0;
            (f(f, subOffset, childNodes, result.subspan(I + offset),
               Children{}),
             ...);
            offset += NumChildren - 1 + subOffset;
        }
    };
    size_t offset = 0;
    (f(f, offset, packed, result, Forest{}), ...);
}

template <auto...>
struct PrintVal;

struct LinearParser: public ParserBase {
    using ParserBase::ParserBase;

    template <size_t N>
        requires(N > 0)
    std::array<Facet const*, N> parseLinearGrammar(
        ParserRule const (&rules)[N], RecoveryOptions const& recoveryOptions) {
        return parseLinearGrammar(std::span(rules), recoveryOptions);
    }

    template <size_t N>
        requires(N > 0)
    std::array<Facet const*, N> parseLinearGrammar(
        std::span<ParserRule const, N> rules,
        RecoveryOptions const& recoveryOptions) {
        std::array<Facet const*, N> facets{};
        parseLinearGrammarImpl(rules, facets, recoveryOptions);
        return facets;
    }

    template <size_t N, typename... Children>
    struct LinParser {
        LinearParser& parser;
        RecoveryOptions recov;
        std::array<ParserRule, N> rules;

        template <typename NewChild = IndexTree<N>>
        LinParser<N + 1, Children..., NewChild> rule(ParserRule rule) const&& {
            LinParser<N + 1, Children..., NewChild> result = { parser, recov };
            ranges::move(rules, result.rules.begin());
            result.rules.back() = std::move(rule);
            return result;
        }

        LinParser<N + 1, Children..., IndexTree<N>> fastFail(
            ParserRule rule) const&& {
            rule.isFastFail = true;
            return std::move(*this).template rule(std::move(rule));
        }

        template <size_t M>
            requires(M > 0)
        auto optRule(ParserRule (&&rules)[M]) const&& {
            rules[0].isFastFail = true;
            return [&]<size_t... I>(std::index_sequence<I...>) {
                return std::move(*this)
                    .template rule<IndexTree<N, IndexTree<I>...>>(
                        parser.option(std::move(rules)));
            }(std::make_index_sequence<M>{});
        }

        auto eval() const&& {
            static constexpr size_t Size = computeUnpackedSize<Children...>(N);
            auto packed = parser.parseLinearGrammar(std::span(rules), recov);
            std::array<Facet const*, Size> result;
            unpackResult<Children...>(packed, result);
            return result;
        }
    };

    LinParser<0> parseLin(RecoveryOptions recov) { return { *this, recov }; }

    template <size_t N>
        requires(N > 0)
    ParserRule option(ParserRule const (&rules)[N]) {
        return { [&]() -> ListFacet const* {
            auto elems = parseLinearGrammarNoRecov(rules);
            if (elems.front()) return allocate<ListFacet>(elems);
            return nullptr;
        } };
    }

private:
    // MARK: Automatic error recovery
    template <size_t N>
    std::array<Facet const*, N> parseLinearGrammarNoRecov(
        ParserRule const (&rules)[N]) {
        auto options = recovOptStack.top(); // Intentionally make a copy
        return parseLinearGrammar(rules, options);
    }

    void parseLinearGrammarImpl(std::span<ParserRule const> rules,
                                std::span<Facet const*> facets,
                                RecoveryOptions const& recoveryOptions);

    std::optional<size_t> recoverLinearGrammar(
        std::span<ParserRule const> rules, std::span<Facet const*> facets,
        RecoveryOptions const& recoveryOptions);

    std::optional<size_t> recoverLinearGrammarImpl(
        std::span<ParserRule const> rules, std::span<Facet const*> facets,
        RecoveryOptions const& recoveryOptions);

    std::pair<Facet const*, size_t> findFacetForRecovery(
        std::span<ParserRule const> rules, bool first);

    utl::stack<RecoveryOptions> recovOptStack;
    bool inRecovery = false;
};

} // namespace prism
