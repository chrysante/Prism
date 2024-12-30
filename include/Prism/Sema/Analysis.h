#ifndef PRISM_SEMA_ANALYSIS_H
#define PRISM_SEMA_ANALYSIS_H

#include <span>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class DiagnosticHandler;

/// Top-level sema function. Constructs and analyzes a module
Target* analyzeModule(MonotonicBufferResource& resource, SemaContext& ctx,
                      DiagnosticHandler& diagHandler,
                      std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_ANALYSIS_H
