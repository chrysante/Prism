#include "Prism/Sema2/NameLookup.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;
using ranges::views::filter;
using ranges::views::join;
using ranges::views::transform;

static NameLookupResult lookup_similar(Scope* scope, std::string_view name) {
    while (scope) {
        auto scope_symbols = scope->symbols_by_approx_name(name);
        if (scope_symbols.empty()) {
            scope = scope->parent_scope();
            continue;
        }
        auto min = ranges::min_element(scope_symbols, ranges::less{},
                                       [=](Symbol const* sym) {
            return utl::levenshtein_distance(sym->name(), name);
        });
        PRISM_ASSERT(min != scope_symbols.end());
        return detail::SimilarName(*min);
    }
    return {};
}

namespace {

struct LookupContext {
    std::string_view name;
    NameLookupOptions options;

    NameLookupResult lookup_unqual(Scope* scope) {
        utl::small_vector<Symbol*> symbols;
        auto* current_scope = scope;
        while (current_scope) {
            auto scope_symbols = search_scope(current_scope);
            symbols.insert(symbols.end(), scope_symbols.begin(),
                           scope_symbols.end());
            if (symbols.size() == 1) return symbols.front();
#if 0
            if (!symbols.empty() && ranges::none_of(symbols, isa<Function>))
                return symbols;
#endif
            current_scope = current_scope->parent_scope();
        }
        if (symbols.empty()) {
            if (options.allow_similar_names)
                return lookup_similar(scope, name);
            else
                return {};
        }
        // Overload set
#if 0
        if (ranges::all_of(symbols, isa<Function>))
            return symbols | transform(cast<Function*>) |
                   ranges::to<utl::small_vector<Function*>>;
#endif
        // Ambiguous
        return symbols;
    }

    utl::small_vector<Symbol*> search_scope(Scope* scope) {
        auto scope_symbols = scope->symbols_by_name(name) | ToSmallVector<>;
#if 0
        if (scope_symbols.empty()) return searchBases(scope);
        if (ranges::any_of(scope_symbols, isa<Function>)) {
            auto baseSymbols = searchBases(scope);
            scope_symbols.insert(scope_symbols.end(), baseSymbols.begin(),
                                baseSymbols.end());
            return scope_symbols;
        }
#endif
        return scope_symbols;
    }

#if 0
    utl::small_vector<Symbol*> searchBases(Scope* scope) {
        auto* sym = scope->defining_symbol();
        if (!sym) return {};
        utl::small_vector<Symbol*> bases;
        // clang-format off
        visit(*sym, csp::overload{
            [&](std::derived_from<CompTypeInterface> auto& type) {
                bases.reserve(
                    type.baseTraits().size() + type.baseClasses().size());
                ranges::copy(type.baseTraits() | transform(FN1(_1->trait())),
                             std::back_inserter(bases));
                ranges::copy(type.baseClasses() | transform(FN1(_1->type())),
                             std::back_inserter(bases));
            },
            [&](std::derived_from<TraitInterface> auto& trait) {
                bases.reserve(trait.baseTraits().size());
                ranges::copy(trait.baseTraits() | transform(FN1(_1->trait())),
                             std::back_inserter(bases));
            },
            [](auto const&) {}
        }); // clang-format on
        utl::small_vector<Symbol*> result;
        ranges::for_each(bases, [&](Symbol* base) {
            result.insert(result.end(), search_scope(base->associatedScope()));
        });
        return result;
    }
#endif
};

} // namespace

NameLookupResult prism::unqualified_lookup(Scope* scope, std::string_view name,
                                           NameLookupOptions options) {
    LookupContext ctx{ name, options };
    return ctx.lookup_unqual(scope);
}
