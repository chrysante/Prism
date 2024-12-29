#ifndef PRISM_SEMA_ANALYSIS_H
#define PRISM_SEMA_ANALYSIS_H

#include <span>

#include <Prism/Sema/SymbolFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class IssueHandler;

/// Top-level sema function. Constructs and analyzes a module
Target* analyzeModule(MonotonicBufferResource& resource, SemaContext& ctx,
                      IssueHandler& issueHandler,
                      std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_ANALYSIS_H
