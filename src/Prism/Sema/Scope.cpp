#include "Prism/Sema/Scope.h"

#include <algorithm>

#include <range/v3/algorithm.hpp>
#include <utl/callback_iterator.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

std::span<Symbol const* const> Scope::symbolsByName(
    std::string_view name) const {
    auto itr = _names.find(name);
    if (itr != _names.end()) return itr->second;
    return {};
}

static size_t computeAcceptedDistance(size_t nameSize) {
    return std::clamp(nameSize / 3, size_t{ 1 }, size_t{ 4 });
}

template <typename S, typename Map>
static utl::small_vector<S*> symbolsByApproxNameImpl(std::string_view name,
                                                     Map const& map) {
    utl::small_vector<S*> result;
    map.lookup(name, computeAcceptedDistance(name.size()),
               utl::callback_iterator([&](auto itr) {
        result.insert(result.end(), itr->value().begin(), itr->value().end());
    }));
    return result;
}

utl::small_vector<Symbol*> Scope::symbolsByApproxName(std::string_view name) {
    return symbolsByApproxNameImpl<Symbol>(name, _approxNames);
}

utl::small_vector<Symbol const*> Scope::symbolsByApproxName(
    std::string_view name) const {
    return symbolsByApproxNameImpl<Symbol const>(name, _approxNames);
}

static bool isGenericInst(Symbol const& symbol) {
    return isa<GenStructTypeInst>(symbol) || isa<GenTraitInst>(symbol) ||
           isa<GenTraitImplInst>(symbol);
}

void Scope::addSymbol(Symbol& symbol) {
    PRISM_ASSERT_AUDIT(!ranges::contains(_symbols, &symbol),
                       "Symbol has already been added to this scope");
    _symbols.push_back(&symbol);
    if (symbol.name().empty() || isGenericInst(symbol)) return;
    _names[symbol.name()].push_back(&symbol);
    _approxNames[symbol.name()].push_back(&symbol);
}
