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

FunctionDef const* Scope::function_by_name_and_sig(
    std::string_view name, GenericSignature const& generic_sig,
    FuncSig const& function_sig) const {
    return _function_overload_map.find({ name, generic_sig, function_sig });
}

void Scope::set_function_signature(GenericSignature generic_sig,
                                   FuncSig function_sig,
                                   FunctionDef* function) {
    bool success = _function_overload_map.insert({ function->name(),
                                                   std::move(generic_sig),
                                                   std::move(function_sig) },
                                                 function);
    PRISM_ASSERT(success, "Function signature is already defined");
}

void Scope::add_symbol(Symbol& symbol) {
    bool participate_in_name_lookup = !symbol.excluded_from_name_lookup();
    add_symbol(symbol, symbol.name(), participate_in_name_lookup);
}

void Scope::add_symbol(Symbol& symbol, std::string const& name,
                       bool participate_in_name_lookup) {
    PRISM_ASSERT_AUDIT(!ranges::contains(_symbols, &symbol),
                       "symbol has already been added to this scope");
    _symbols.push_back(&symbol);
    if (participate_in_name_lookup && !name.empty()) {
        _names[name].push_back(&symbol);
        _approx_names[name].push_back(&symbol);
    }
}

void Scope::set_defining_symbol(Symbol* def_symbol) {
    PRISM_ASSERT(def_symbol);
    _defining_symbol = def_symbol;
    PRISM_ASSERT(_parent_scope == nullptr ||
                 _parent_scope == def_symbol->parent_scope());
    _parent_scope = def_symbol->parent_scope();
}
