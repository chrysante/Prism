#include "Prism/Sema2/Scope.h"

#include <algorithm>

#include <range/v3/algorithm.hpp>
#include <utl/callback_iterator.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

std::span<Symbol const* const> Scope::symbols_by_name(
    std::string_view name) const {
    auto itr = _names.find(name);
    if (itr != _names.end()) return itr->second;
    return {};
}

static size_t compute_accepted_distance(size_t nameSize) {
    return std::clamp(nameSize / 3, size_t{ 1 }, size_t{ 4 });
}

template <typename S, typename Map>
static utl::small_vector<S*> symbols_by_approx_name_impl(std::string_view name,
                                                         Map const& map) {
    utl::small_vector<S*> result;
    map.lookup(name, compute_accepted_distance(name.size()),
               utl::callback_iterator([&](auto itr) {
        result.insert(result.end(), itr->value().begin(), itr->value().end());
    }));
    return result;
}

utl::small_vector<Symbol*> Scope::symbols_by_approx_name(
    std::string_view name) {
    return symbols_by_approx_name_impl<Symbol>(name, _approx_names);
}

utl::small_vector<Symbol const*> Scope::symbols_by_approx_name(
    std::string_view name) const {
    return symbols_by_approx_name_impl<Symbol const>(name, _approx_names);
}

void Scope::add_symbol(Symbol& symbol) {
    PRISM_ASSERT_AUDIT(!ranges::contains(_symbols, &symbol),
                       "symbol has already been added to this scope");
    _symbols.push_back(&symbol);
    if (!symbol.name().empty() && !symbol.excluded_from_name_lookup()) {
        _names[symbol.name()].push_back(&symbol);
        _approx_names[symbol.name()].push_back(&symbol);
    }
}

void Scope::set_defining_symbol(Symbol* def_symbol) {
    PRISM_ASSERT(def_symbol);
    _defining_symbol = def_symbol;
    PRISM_ASSERT(_parent_scope == nullptr ||
                 _parent_scope == def_symbol->parent_scope());
    _parent_scope = def_symbol->parent_scope();
}
