#ifndef PRISM_SEMA_FUNCTIONANALYSIS_H
#define PRISM_SEMA_FUNCTIONANALYSIS_H

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class DiagnosticHandler;

/// Semantically analyzes the function \p func
void analyzeFunction(MonotonicBufferResource&, SemaContext& ctx,
                     DiagnosticHandler& diagHandler, FunctionImpl& func);

/// Semantically analyzes all global functions in \p target
void analyzeTargetFunctions(MonotonicBufferResource&, SemaContext& ctx,
                            DiagnosticHandler& diagHandler, Target& target);

} // namespace prism

#endif // PRISM_SEMA_FUNCTIONANALYSIS_H
