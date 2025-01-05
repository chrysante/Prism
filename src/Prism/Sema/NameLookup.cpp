#include "Prism/Sema/NameLookup.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::filter;
using ranges::views::transform;

static NameLookupResult lookupSimilar(Scope* scope, std::string_view name) {
    while (scope) {
        auto scopeSymbols = scope->symbolsByApproxName(name);
        if (scopeSymbols.empty()) {
            scope = scope->parent();
            continue;
        }
        auto min = ranges::min_element(scopeSymbols, ranges::less{},
                                       [=](Symbol const* sym) {
            return utl::levenshtein_distance(sym->name(), name);
        });
        PRISM_ASSERT(min != scopeSymbols.end());
        return detail::SimilarName(*min);
    }
    return {};
}

NameLookupResult prism::unqualifiedLookup(Scope* const scope,
                                          std::string_view name,
                                          NameLookupOptions options) {
    utl::small_vector<Symbol*> symbols;
    bool isOverloadSet;
    auto* currentScope = scope;
    while (currentScope) {
        auto scopeSymbols = currentScope->symbolsByName(name);
        if (ranges::any_of(scopeSymbols, isa<Function>)) {
            isOverloadSet = true;
            ranges::copy(scopeSymbols | filter(isa<Function>),
                         std::back_inserter(symbols));
        }
        else {
            ranges::copy(scopeSymbols, std::back_inserter(symbols));
        }
        if (isOverloadSet || scopeSymbols.empty()) {
            currentScope = currentScope->parent();
            continue;
        }
        if (scopeSymbols.size() == 1) return scopeSymbols.front();
        return scopeSymbols | ToSmallVector<>;
    }
    if (symbols.empty()) {
        if (options.allowSimilarNames)
            return lookupSimilar(scope, name);
        else
            return {};
    }
    if (!isOverloadSet) // Ambiguous case
        return symbols;
    if (symbols.size() == 1) return symbols.front();
    return symbols | transform(cast<Function*>) |
           ranges::to<utl::small_vector<Function*>>;
}
