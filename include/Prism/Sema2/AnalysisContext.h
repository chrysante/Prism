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

/// Returns the facet of \p symbol in \p scope
/// This function exists because not all symbols are unique to their source
/// location. Generic parameters, literals etc. are shared between scope,
/// for these symbols we maintain maps to lookup their facets in a given scope.
Facet const* get_facet(Symbol const& symbol, Scope const* scope);

} // namespace prism

#endif // PRISM_SEMA2_ANALYSISCONTEXT_H
