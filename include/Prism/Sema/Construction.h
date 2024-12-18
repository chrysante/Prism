#ifndef PRISM_SEMA_CONSTRUCTION_H
#define PRISM_SEMA_CONSTRUCTION_H

#include <span>
#include <utility>

namespace prism {

class SemaContext;
class SourceContext;
class Target;
class SourceFileFacet;

struct SourceFilePair {
    SourceFileFacet const* facet;
    SourceContext const& context;
};

Target* constructTarget(SemaContext& ctx,
                        std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_CONSTRUCTION_H
