#ifndef PRISM_SEMA2_NAMELOOKUP_H
#define PRISM_SEMA2_NAMELOOKUP_H

#include <span>
#include <string_view>
#include <variant>

#include <utl/vector.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class Scope;

// FIXME: Delete this
class Function;

/// Argument structure for `unqualified_lookup()`
struct NameLookupOptions {
    /// Set to search return similar names in the result if no direct match was
    /// found
    bool allow_similar_names = true;
};

/// Performs unqualified name lookup in \p scope
class NameLookupResult unqualified_lookup(Scope* scope, std::string_view name,
                                          NameLookupOptions options = {});

namespace detail {

struct SimilarName {
    Symbol* symbol;
};

} // namespace detail

/// Result structure for unqualified name
class NameLookupResult {
    using OverloadSet = utl::small_vector<Function*>;
    using AmbiSet = utl::small_vector<Symbol*>;

public:
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

    std::span<Function* const> overload_set() const {
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

    template <typename Vis, typename R = std::common_reference_t<
                                std::invoke_result_t<Vis, None>,
                                std::invoke_result_t<Vis, Symbol*>,
                                std::invoke_result_t<Vis, OverloadSet>,
                                std::invoke_result_t<Vis, AmbiSet>,
                                std::invoke_result_t<Vis, Similar>>>
    R visit(Vis&& vis) const {
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
