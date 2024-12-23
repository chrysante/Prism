#ifndef PRISM_SEMA_ANALYSISBASE_H
#define PRISM_SEMA_ANALYSISBASE_H

namespace prism {

class SemaContext;
class IssueHandler;
class SourceContext;

struct AnalysisBase {
    SemaContext& ctx;
    IssueHandler& iss;
    SourceContext const* sourceContext = nullptr;
};

} // namespace prism

#endif // PRISM_SEMA_ANALYSISBASE_H
