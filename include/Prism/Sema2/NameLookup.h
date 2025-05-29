#ifndef PRISM_SEMA2_NAMELOOKUP_H
#define PRISM_SEMA2_NAMELOOKUP_H

#include <span>
#include <string_view>
#include <variant>

#include <utl/vector.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class Scope;
class NameLookupResult;

/// Argument structure for `unqualified_lookup()`
struct NameLookupOptions {
    /// Set to search return similar names in the result if no direct match was
    /// found
    bool allow_similar_names = true;
};

/// Performs unqualified name lookup in \p scope
NameLookupResult unqualified_lookup(Scope* scope, std::string_view name,
                                    NameLookupOptions options = {});

namespace detail {

struct SimilarName {
    Symbol* symbol;
};

} // namespace detail

/// Result structure for unqualified name
class NameLookupResult {
public:
    struct OverloadSet: utl::small_vector<Symbol*> {
        using small_vector::small_vector;
    };
    struct AmbiSet: utl::small_vector<Symbol*> {
        using small_vector::small_vector;
    };
    using None = std::monostate;
    using Similar = detail::SimilarName;

    NameLookupResult() = default;

    NameLookupResult(Symbol* symbol): data(symbol) {}

    NameLookupResult(OverloadSet overload_set): data(std::move(overload_set)) {}

    NameLookupResult(AmbiSet ambiSet): data(std::move(ambiSet)) {}

    NameLookupResult(Similar similar): data(similar) {}

    bool is_none() const { return is<None>(); }

    bool is_single_symbol() const { return is<Symbol*>(); }

    Symbol* single_symbol() const {
        return is_single_symbol() ? get<Symbol*>() : nullptr;
    }

    bool is_overload_set() const { return is<OverloadSet>(); }

    std::span<Symbol* const> overload_set() const {
        if (is_overload_set()) return get<OverloadSet>();
        return {};
    }

    bool is_ambiguous() const { return is<AmbiSet>(); }

    std::span<Symbol* const> ambiguous_symbols() const {
        if (is_ambiguous()) return get<AmbiSet>();
        return {};
    }

    bool is_similar() const { return is<Similar>(); }

    Symbol* similar() const {
        return is_similar() ? get<Similar>().symbol : nullptr;
    }

    bool success() const {
        return !is_none() && !is_similar() && !is_ambiguous();
    }

    template <typename Vis>
    decltype(auto) visit(Vis&& vis) {
        return std::visit(vis, data);
    }

    template <typename Vis>
    decltype(auto) visit(Vis&& vis) const {
        return std::visit(vis, data);
    }

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

} // namespace prism

#endif // PRISM_SEMA2_NAMELOOKUP_H
