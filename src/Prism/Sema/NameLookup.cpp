#include "Prism/Sema/NameLookup.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::filter;
using ranges::views::transform;

NameLookupResult prism::unqualifiedLookup(Scope* scope, std::string_view name) {
    utl::small_vector<Symbol*> symbols;
    bool isOverloadSet;
    while (scope) {
        auto scopeSymbols = scope->symbols() | filter([&](auto* sym) {
            return sym->name() == name;
        }) | ranges::to<utl::small_vector<Symbol*>>;
        if (ranges::any_of(scopeSymbols, isa<Function>)) {
            isOverloadSet = true;
            ranges::copy(scopeSymbols | filter(isa<Function>),
                         std::back_inserter(symbols));
        }
        else {
            ranges::copy(scopeSymbols, std::back_inserter(symbols));
        }
        if (isOverloadSet || scopeSymbols.empty()) {
            scope = scope->assocSymbol()->parentScope();
            continue;
        }
        if (scopeSymbols.size() == 1) return scopeSymbols.front();
        return scopeSymbols;
    }
    if (symbols.empty()) return {};
    if (isOverloadSet)
        return symbols | transform(cast<Function*>) |
               ranges::to<utl::small_vector<Function*>>;
    return symbols;
}
