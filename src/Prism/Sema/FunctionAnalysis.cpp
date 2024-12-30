#include "Prism/Sema/FunctionAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/ExprAnalysis.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::transform;

namespace {

struct FuncAnaCtx: AnalysisBase {
    FunctionImpl& func;
    Scope* currScope = nullptr;

    void run();

    void analyze(StmtFacet const* facet);

    void doAnalyze(StmtFacet const&) {}
    void doAnalyze(ExprStmtFacet const&);
    void doAnalyze(ReturnStmtFacet const&);
};

} // namespace

void prism::analyzeFunction(MonotonicBufferResource&, SemaContext& ctx,
                            IssueHandler& iss, FunctionImpl& func) {
    FuncAnaCtx{ { ctx, iss, getSourceContext(&func) }, func }.run();
}

void prism::analyzeTargetFunctions(MonotonicBufferResource& resource,
                                   SemaContext& ctx, IssueHandler& iss,
                                   Target& target) {
    auto dfs = [&](auto& dfs, Scope* scope) -> void {
        for (auto* sym: scope->symbols()) {
            if (isa<SourceFile>(sym) || isa<CompositeType>(sym) ||
                isa<Trait>(sym) || isa<TraitImpl>(sym))
                dfs(dfs, sym->associatedScope());
            if (auto* func = dyncast<FunctionImpl*>(sym))
                analyzeFunction(resource, ctx, iss, *func);
        }
    };
    dfs(dfs, target.associatedScope());
}

void FuncAnaCtx::run() {
    auto* facet = func.facet();
    auto* body = cast<CompoundFacet const*>(facet->body());
    currScope = func.associatedScope();
    ranges::for_each(body->statements()->elems(), FN1(&, analyze(_1)));
}

void FuncAnaCtx::analyze(StmtFacet const* facet) {
    if (facet) visit(*facet, FN1(&, doAnalyze(_1)));
}

void FuncAnaCtx::doAnalyze(ExprStmtFacet const&) {}

void FuncAnaCtx::doAnalyze(ReturnStmtFacet const& facet) {
    auto* value = analyzeFacetAs<Value>(*this, currScope, facet.expr());
    ctx.make<RetInst>(currScope, &facet, value);
}
