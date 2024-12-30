#ifndef PRISM_SEMA_CONSTRUCTION_H
#define PRISM_SEMA_CONSTRUCTION_H

#include <optional>
#include <span>

#include <Prism/Common/Allocator.h>
#include <Prism/Sema/DependencyGraph.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaContext;
class SourceContext;
class SourceFileFacet;
class IssueHandler;

struct ConstructionResult {
    static ConstructionResult Fatal(Target* target) {
        return { .target = target, .haveFatalError = true };
    }

    Target* target;
    std::optional<DependencyGraph> dependencyGraph = {};
    bool haveFatalError = false;
};

ConstructionResult constructTarget(MonotonicBufferResource& resource,
                                   SemaContext& ctx, IssueHandler& issueHandler,
                                   std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_CONSTRUCTION_H
