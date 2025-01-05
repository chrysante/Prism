#ifndef PRISM_SEMA_ANALYSIS_H
#define PRISM_SEMA_ANALYSIS_H

#include <span>

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;

/// Top-level sema function. Constructs and analyzes a module
Target* analyzeModule(MonotonicBufferResource& resource, SemaContext& ctx,
                      DiagnosticEmitter& DE,
                      std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_ANALYSIS_H
