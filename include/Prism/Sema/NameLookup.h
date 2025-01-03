#ifndef PRISM_SEMA_NAMELOOKUP_H
#define PRISM_SEMA_NAMELOOKUP_H

#include <span>
#include <string_view>
#include <variant>

#include <utl/vector.hpp>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class Scope;

namespace detail {

struct SimilarName {
    Symbol* symbol;
};

} // namespace detail

class NameLookupResult {
    using OverloadSet = utl::small_vector<Function*>;
    using AmbiSet = utl::small_vector<Symbol*>;
    using None = std::monostate;
    using Similar = detail::SimilarName;

public:
    NameLookupResult() = default;

    NameLookupResult(Symbol* symbol): data(symbol) {}

    NameLookupResult(OverloadSet overloadSet): data(std::move(overloadSet)) {}

    NameLookupResult(AmbiSet ambiSet): data(std::move(ambiSet)) {}

    NameLookupResult(Similar similar): data(similar) {}

    bool isNone() const { return is<None>(); }

    bool isSingleSymbol() const { return is<Symbol*>(); }

    Symbol* singleSymbol() const {
        return isSingleSymbol() ? get<Symbol*>() : nullptr;
    }

    bool isOverloadSet() const { return is<OverloadSet>(); }

    std::span<Function* const> overloadSet() const {
        if (isOverloadSet()) return get<OverloadSet>();
        return {};
    }

    bool isAmbiguous() const { return is<AmbiSet>(); }

    std::span<Symbol* const> ambiguousSymbols() const {
        if (isAmbiguous()) return get<AmbiSet>();
        return {};
    }

    bool isSimilar() const { return is<Similar>(); }

    Symbol* similar() const {
        return isSimilar() ? get<Similar>().symbol : nullptr;
    }

    bool success() const { return !isNone() && !isSimilar() && !isAmbiguous(); }

private:
    template <typename T>
    bool is() const {
        return std::holds_alternative<T>(data);
    }
    template <typename T>
    T const& get() const {
        return std::get<T>(data);
    }

    std::variant<None, Symbol*, OverloadSet, AmbiSet, Similar> data;
};

struct NameLookupOptions {
    bool allowSimilarNames = true;
};

NameLookupResult unqualifiedLookup(Scope* scope, std::string_view name,
                                   NameLookupOptions options = {});

} // namespace prism

#endif // PRISM_SEMA_NAMELOOKUP_H
