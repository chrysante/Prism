#ifndef PRISM_SEMA_ANALYSISBASE_H
#define PRISM_SEMA_ANALYSISBASE_H

#include <span>

#include <Prism/Diagnostic/DiagnosticEmitter.h>

namespace prism {

class SemaContext;
class SourceContext;
class Symbol;
class Facet;

struct LazySymbolInstantiation {
    Symbol* symbol = nullptr;
    Facet const* facet = nullptr;
};

class AnalysisBase {
public:
    SemaContext& ctx;
    DiagnosticEmitter& DE;
    SourceContext const* sourceContext = nullptr;
};

/// \Return the source context in which \p symbol is defined
SourceContext const* getSourceContext(Symbol const* symbol);

} // namespace prism

#endif // PRISM_SEMA_ANALYSISBASE_H
