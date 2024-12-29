#ifndef PRISM_SEMA_CONFORMANCEANALYSIS_H
#define PRISM_SEMA_CONFORMANCEANALYSIS_H

#include <Prism/Sema/SymbolFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class IssueHandler;
class DependencyGraph;

/// Analysis trait and base class conformances
void analyzeConformances(MonotonicBufferResource& resource, SemaContext& ctx,
                         IssueHandler& iss, Target& target,
                         DependencyGraph const& dependencyGraph);

} // namespace prism

#endif // PRISM_SEMA_CONFORMANCEANALYSIS_H
