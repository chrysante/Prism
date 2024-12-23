#ifndef PRISM_SEMA_EXPRANALYSIS_H
#define PRISM_SEMA_EXPRANALYSIS_H

#include <concepts>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/SymbolFwd.h>

namespace prism {

class Facet;
class AnalysisBase;
class Scope;

Symbol* analyzeFacet(AnalysisBase const& context, Scope* scope,
                     Facet const* facet);

namespace detail {

template <std::derived_from<Symbol> S>
S* verifySymbolType(AnalysisBase const& context, Symbol* symbol) {
    auto* derived = dyncast<S*>(symbol);
    if (derived) return derived;
    PRISM_UNIMPLEMENTED();
    return nullptr;
}

} // namespace detail

template <std::derived_from<Symbol> S>
S* analyzeFacetAs(AnalysisBase const& context, Scope* scope,
                  Facet const* facet) {
    auto* symbol = analyzeFacet(context, scope, facet);
    if (!symbol) return nullptr;
    return detail::verifySymbolType<S>(context, symbol);
}

} // namespace prism

#endif // PRISM_SEMA_EXPRANALYSIS_H
