#ifndef PRISM_SEMA2_ANALYSISCONTEXT_H
#define PRISM_SEMA2_ANALYSISCONTEXT_H

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema2/SubContext.h>

namespace prism {

class SemaContext;
class SourceContext;
class Symbol;
class Facet;

/// Base class for semantic analysis contexts that provides commonly required
/// members
class AnalysisContext {
public:
    SemaContext& ctx;
    DiagnosticEmitter& DE;
    SourceContext const* source_context = nullptr;
    SubContext sub_context{};
};

} // namespace prism

#endif // PRISM_SEMA2_ANALYSISCONTEXT_H
