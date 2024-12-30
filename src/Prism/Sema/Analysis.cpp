#include "Prism/Sema/Analysis.h"

#include "Prism/Sema/ConformanceAnalysis.h"
#include "Prism/Sema/Construction.h"
#include "Prism/Sema/FunctionAnalysis.h"

using namespace prism;

Target* prism::analyzeModule(MonotonicBufferResource& resource,
                             SemaContext& ctx, IssueHandler& iss,
                             std::span<SourceFilePair const> input) {
    auto constr = constructTarget(resource, ctx, iss, input);
    if (constr.haveFatalError) return constr.target;
    auto* target = constr.target;
    auto& dependencies = constr.dependencyGraph.value();
    analyzeConformances(resource, ctx, iss, *target, dependencies);
    analyzeTargetFunctions(resource, ctx, iss, *target);
    return target;
}
