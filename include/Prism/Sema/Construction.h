#ifndef PRISM_SEMA_CONSTRUCTION_H
#define PRISM_SEMA_CONSTRUCTION_H

#include <span>

#include <Prism/Common/Allocator.h>
#include <Prism/Sema/SymbolFwd.h>

namespace prism {

class SemaContext;
class SourceContext;
class SourceFileFacet;
class IssueHandler;

Target* constructTarget(MonotonicBufferResource& resource, SemaContext& ctx,
                        IssueHandler& issueHandler,
                        std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_CONSTRUCTION_H
