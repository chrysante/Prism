#ifndef PRISM_SEMA_CONSTRUCTION_H
#define PRISM_SEMA_CONSTRUCTION_H

#include <span>
#include <utility>

namespace prism {

class SemaContext;
class SourceContext;
class Target;
class SourceFileFacet;
class IssueHandler;

struct SourceFilePair {
    SourceFileFacet const* facet;
    SourceContext const& context;
};

Target* constructTarget(SemaContext& ctx, IssueHandler& issueHandler,
                        std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_CONSTRUCTION_H
