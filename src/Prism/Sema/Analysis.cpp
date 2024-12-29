#include "Prism/Sema/Analysis.h"

#include "Prism/Sema/Construction.h"

using namespace prism;

Target* prism::analyzeModule(MonotonicBufferResource& resource,
                             SemaContext& ctx, IssueHandler& iss,
                             std::span<SourceFilePair const> input) {
    auto* target = constructTarget(resource, ctx, iss, input);
    return target;
}
