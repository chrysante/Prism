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

using NLR = NameLookupResult;

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

static bool is_function_like(Symbol* sym) {
    return isa<Function>(sym) || isa<FunctionDef>(sym);
}

namespace {

struct LookupContext {
    std::string_view name;
    NameLookupOptions options;

    NameLookupResult lookup_unqual(Scope* scope) {
        for (auto* current_scope = scope; current_scope;
             current_scope = current_scope->parent_scope())
        {
            auto symbols = search_scope(current_scope);
            if (symbols.empty()) continue;
            if (symbols.size() == 1) return symbols.front();
            if (ranges::all_of(symbols, is_function_like))
                return NLR::OverloadSet{ symbols.begin(), symbols.end() };
            return NLR::AmbiSet{ symbols.begin(), symbols.end() };
        }
        if (options.allow_similar_names) return lookup_similar(scope, name);
        return {};
    }

    std::span<Symbol* const> search_scope(Scope* scope) {
        auto scope_symbols = scope->symbols_by_name(name);
#if 0 // Search base classes
        if (scope_symbols.empty()) return search_bases(scope);
        if (ranges::any_of(scope_symbols, isa<Function>)) {
            auto base_symbols = search_bases(scope);
            scope_symbols.insert(scope_symbols.end(), base_symbols.begin(),
                                 base_symbols.end());
            return scope_symbols;
        }
#endif
        return scope_symbols;
    }

#if 0
    utl::small_vector<Symbol*> search_bases(Scope* scope) {
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
