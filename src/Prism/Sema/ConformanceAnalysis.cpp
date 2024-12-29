#include "Prism/Sema/ConformanceAnalysis.h"

#include "Prism/Common/IssueHandler.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

void prism::analyzeConformances(MonotonicBufferResource& resource,
                                SemaContext& ctx, IssueHandler& iss,
                                Target& target,
                                DependencyGraph const& dependencies) {
    generateGraphvizDebug(dependencies);
}
