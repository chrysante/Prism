#ifndef PRISM_SEMA_EXPRANALYSIS_H
#define PRISM_SEMA_EXPRANALYSIS_H

#include <Prism/Sema/SymbolFwd.h>

namespace prism {

class Facet;
class SemaContext;
class SourceContext;

Symbol* analyzeFacet(SemaContext& ctx, Facet const*,
                     SourceContext const& sourceContext);

Symbol* analyzeTypeFacet(SemaContext& ctx, Facet const*,
                         SourceContext const& sourceContext);

} // namespace prism

#endif // PRISM_SEMA_EXPRANALYSIS_H
