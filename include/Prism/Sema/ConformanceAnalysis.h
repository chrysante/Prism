#ifndef PRISM_SEMA_CONFORMANCEANALYSIS_H
#define PRISM_SEMA_CONFORMANCEANALYSIS_H

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class DiagnosticEmitter;

void analyze_trait_conformances(SemaContext& ctx, DiagnosticEmitter& DE,
                                Module& mod);

} // namespace prism

#endif // PRISM_SEMA_CONFORMANCEANALYSIS_H
