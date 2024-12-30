#ifndef PRISM_SEMA_ANALYSISBASE_H
#define PRISM_SEMA_ANALYSISBASE_H

namespace prism {

class SemaContext;
class IssueHandler;
class SourceContext;
class Symbol;

class AnalysisBase {
public:
    SemaContext& ctx;
    IssueHandler& iss;
    SourceContext const* sourceContext = nullptr;
};

/// \Return the source context in which \p symbol is defined
SourceContext const* getSourceContext(Symbol const* symbol);

} // namespace prism

#endif // PRISM_SEMA_ANALYSISBASE_H
