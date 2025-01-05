#ifndef PRISM_SEMA_CONSTRUCTION_H
#define PRISM_SEMA_CONSTRUCTION_H

#include <optional>
#include <span>
#include <vector>

#include <Prism/Common/Allocator.h>
#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema/AnalysisBase.h>
#include <Prism/Sema/DependencyGraph.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaContext;
class SourceContext;
class SourceFileFacet;

struct ConstructionResult {
    static ConstructionResult Fatal(Target* target) {
        return { .target = target, .haveFatalError = true };
    }

    Target* target;
    std::optional<DependencyGraph> dependencyGraph = {};
    std::vector<LazySymbolInstantiation> lazyInstantiations;
    bool haveFatalError = false;
};

ConstructionResult constructTarget(MonotonicBufferResource& resource,
                                   SemaContext& ctx, DiagnosticEmitter& DE,
                                   std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_CONSTRUCTION_H
