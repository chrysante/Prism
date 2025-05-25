#ifndef PRISM_SEMA2_FUNCTIONANALYSIS_H
#define PRISM_SEMA2_FUNCTIONANALYSIS_H

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SemaContext;
class DiagnosticEmitter;

/// Analyze function bodies
void analyze_functions(SemaContext& ctx, DiagnosticEmitter& DE, Module& mod);

} // namespace prism

#endif // PRISM_SEMA2_FUNCTIONANALYSIS_H
