#ifndef PRISM_SEMA_FUNCTIONANALYSIS_H
#define PRISM_SEMA_FUNCTIONANALYSIS_H

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class DiagnosticEmitter;

/// Analyze function bodies
void analyze_functions(SemaContext& ctx, DiagnosticEmitter& DE, Module& mod);

} // namespace prism

#endif // PRISM_SEMA_FUNCTIONANALYSIS_H
