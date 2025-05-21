#include "Prism/Sema2/SemaContext.h"

#include <bit>

#include <range/v3/algorithm.hpp>
#include <utl/hashtable.hpp>

#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

struct SemaContext::Impl {
    std::vector<csp::unique_ptr<Symbol>> symbol_bag;
    std::vector<std::unique_ptr<Scope>> scope_bag;
    utl::hashmap<SourceFileFacet const*, SourceContext const*>
        source_context_map;
};

SemaContext::SemaContext() = default;

SemaContext::~SemaContext() = default;

Scope* SemaContext::make_scope(Symbol* defining_symbol) {
    impl->scope_bag.push_back(std::make_unique<Scope>(defining_symbol));
    return impl->scope_bag.back().get();
}

SourceContext const* SemaContext::get_source_context(Facet const* facet) const {
    while (facet) {
        if (auto* file = dyncast<SourceFileFacet const*>(facet)) {
            auto itr = impl->source_context_map.find(file);
            return itr != impl->source_context_map.end() ? itr->second :
                                                           nullptr;
        }
        facet = facet->parent();
    }
    return nullptr;
}

Symbol* SemaContext::add_symbol(csp::unique_ptr<Symbol> sym) {
    auto* s = sym.get();
    impl->symbol_bag.push_back(std::move(sym));
    return s;
}

void SemaContext::map_source_to_context(SourceFileFacet const* facet,
                                        SourceContext const* ctx) {
    auto [itr, result] = impl->source_context_map.insert({ facet, ctx });
    PRISM_ASSERT(result, "Failed to insert source file. Was it added twice?");
}
