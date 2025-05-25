#include "Prism/Sema2/FunctionAnalysis.h"

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/ExprAnalysis.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

namespace {

struct FuncAnaCtx: AnalysisContext, InstructionEmitter {
    FunctionDef& function;

    FuncAnaCtx(SemaContext& ctx, DiagnosticEmitter& DE, FunctionDef& function):
        AnalysisContext{ ctx, DE, ctx.get_source_context(function.facet()) },
        function(function) {}

    void run();

    void emit_instruction(Instruction*) final {
        PRISM_UNIMPLEMENTED();
    };

    [[nodiscard]] Value* analyze_expr(Facet const* expr_facet, Scope* scope) {
        return prism::analyze_facet_as<Value>(*this, *this, scope, expr_facet);
    }

    void analyze(Facet const* facet, Scope* parent_scope) {
        if (!facet) return;
        visit(*facet, FN1(&, do_analyze(_1, parent_scope)));
    }

    void do_analyze(Facet const&, Scope*) { PRISM_UNREACHABLE(); }

    void do_analyze(ExprStmtFacet const& stmt_facet, Scope* parent_scope) {
        (void)analyze_expr(stmt_facet.expr(), parent_scope);
    }
};

} // namespace

void FuncAnaCtx::run() {
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
                FuncAnaCtx{ ctx, DE, *function }.run();
            if (isa<SourceFile>(sym) || isa<StructDef>(sym) ||
                isa<TraitDef>(sym))
                dfs(dfs, sym->scope());
        }
    };
    dfs(dfs, mod.scope());
}
