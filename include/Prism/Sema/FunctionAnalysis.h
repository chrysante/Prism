#ifndef PRISM_SEMA_FUNCTIONANALYSIS_H
#define PRISM_SEMA_FUNCTIONANALYSIS_H

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;

/// Semantically analyzes the function \p func
void analyzeFunction(MonotonicBufferResource&, SemaContext& ctx,
                     DiagnosticEmitter& DE, FunctionImpl& func);

/// Semantically analyzes all global functions in \p target
void analyzeTargetFunctions(MonotonicBufferResource&, SemaContext& ctx,
                            DiagnosticEmitter& DE, Target& target);

} // namespace prism

#endif // PRISM_SEMA_FUNCTIONANALYSIS_H
