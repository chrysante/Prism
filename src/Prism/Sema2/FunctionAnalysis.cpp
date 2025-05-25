#include "Prism/Sema2/FunctionAnalysis.h"

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/ExprAnalysis.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

namespace {

struct FuncAnaCtx: AnalysisContext {
    void run(FunctionDef& function);

    void analyze(Facet const* facet, Scope* parent_scope) {
        if (!facet) return;
        visit(*facet, FN1(&, do_analyze(_1, parent_scope)));
    }

    void do_analyze(Facet const&, Scope*) { PRISM_UNREACHABLE(); }

    void do_analyze(ExprStmtFacet const& stmt_facet, Scope* parent_scope) {
        analyze(stmt_facet.expr(), parent_scope);
    }

    void do_analyze(CallFacet const& call_facet, Scope* parent_scope) {
        PRISM_UNIMPLEMENTED();
    }
};

} // namespace

void FuncAnaCtx::run(FunctionDef& function) {
    if (auto* def_facet = dyncast<FuncDefFacet const*>(function.facet()))
        if (auto* body = def_facet->body())
            for (auto* elem: body->statements()->elems())
                analyze(elem, function.scope());
}

void prism::analyze_functions(SemaContext& ctx, DiagnosticEmitter& DE,
                              Module& mod) {
    auto dfs = [&](auto& dfs, Scope* scope) -> void {
        if (!scope) return;
        for (auto* sym: scope->symbols()) {
            if (auto* function = dyncast<FunctionDef*>(sym))
                FuncAnaCtx{ ctx, DE }.run(*function);
            if (isa<SourceFile>(sym) || isa<StructDef>(sym) ||
                isa<TraitDef>(sym))
                dfs(dfs, sym->scope());
        }
    };
    dfs(dfs, mod.scope());
}
