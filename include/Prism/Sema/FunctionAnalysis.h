#ifndef PRISM_SEMA_FUNCTIONANALYSIS_H
#define PRISM_SEMA_FUNCTIONANALYSIS_H

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class IssueHandler;

/// Semantically analyzes the function \p func
void analyzeFunction(MonotonicBufferResource&, SemaContext& ctx,
                     IssueHandler& iss, FunctionImpl& func);

/// Semantically analyzes all global functions in \p target
void analyzeTargetFunctions(MonotonicBufferResource&, SemaContext& ctx,
                            IssueHandler& iss, Target& target);

} // namespace prism

#endif // PRISM_SEMA_FUNCTIONANALYSIS_H
