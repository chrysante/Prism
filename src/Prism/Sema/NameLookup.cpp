#include "Prism/Sema/NameLookup.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::filter;
using ranges::views::join;
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

namespace {

struct LookupContext {
    std::string_view name;
    NameLookupOptions options;

    NameLookupResult lookupUnqual(Scope* scope) {
        utl::small_vector<Symbol*> symbols;
        auto* currentScope = scope;
        while (currentScope) {
            auto scopeSymbols = searchScope(currentScope);
            symbols.insert(symbols.end(), scopeSymbols.begin(),
                           scopeSymbols.end());
            if (symbols.size() == 1) return symbols.front();
            if (!symbols.empty() && ranges::none_of(symbols, isa<Function>))
                return symbols;
            currentScope = currentScope->parent();
        }
        if (symbols.empty()) {
            if (options.allowSimilarNames)
                return lookupSimilar(scope, name);
            else
                return {};
        }
        // Overload set
        if (ranges::all_of(symbols, isa<Function>))
            return symbols | transform(cast<Function*>) |
                   ranges::to<utl::small_vector<Function*>>;
        // Ambiguous
        return symbols;
    }

    utl::small_vector<Symbol*> searchScope(Scope* scope) {
        auto scopeSymbols = scope->symbolsByName(name) | ToSmallVector<>;
        if (scopeSymbols.empty()) return searchBases(scope);
        if (ranges::any_of(scopeSymbols, isa<Function>)) {
            auto baseSymbols = searchBases(scope);
            scopeSymbols.insert(scopeSymbols.end(), baseSymbols.begin(),
                                baseSymbols.end());
            return scopeSymbols;
        }
        return scopeSymbols;
    }

    utl::small_vector<Symbol*> searchBases(Scope* scope) {
        auto* sym = scope->assocSymbol();
        if (!sym) return {};
        utl::small_vector<Symbol*> bases;
        visit(*sym, csp::overload{
                        [&](std::derived_from<CompTypeInterface> auto& type) {
            ranges::copy(type.baseTraits() | transform(FN1(_1->trait())),
                         std::back_inserter(bases));
            ranges::copy(type.baseClasses() | transform(FN1(_1->type())),
                         std::back_inserter(bases));
        }, [&](std::derived_from<TraitInterface> auto& trait) {
            ranges::copy(trait.baseTraits() | transform(FN1(_1->trait())),
                         std::back_inserter(bases));
        }, [](auto const&) {} });
        return bases | transform(FN1(&, searchScope(_1->associatedScope()))) |
               join | ToSmallVector<>;
    }
};

} // namespace

NameLookupResult prism::unqualifiedLookup(Scope* scope, std::string_view name,
                                          NameLookupOptions options) {
    LookupContext ctx{ name, options };
    return ctx.lookupUnqual(scope);
}
