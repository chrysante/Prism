#include "Prism/Sema2/AnalysisContext.h"

#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

Facet const* prism::get_facet(Symbol const& symbol, Scope const* scope) {
    if (auto* facet = symbol.facet()) return facet;
    for (; scope; scope = scope->parent_scope())
        if (auto* facet = scope->get_facet(&symbol)) return facet;
    PRISM_UNREACHABLE();
}
