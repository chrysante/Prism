#include "Prism/Sema2/FunctionAnalysis.h"

#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/ExprAnalysis.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

namespace prism {

struct FuncAnaCtx: AnalysisContext, InstructionEmitter {
    FunctionDef& function;
    Scope* scope;
    std::vector<Instruction*>* instructions = nullptr;

    FuncAnaCtx(SemaContext& ctx, DiagnosticEmitter& DE, FunctionDef& function):
        AnalysisContext{ ctx, DE, ctx.get_source_context(function.facet()) },
        function(function),
        scope(function.scope()) {}

    void run();

    void emit_instruction(Instruction* inst) final {
        PRISM_ASSERT(instructions);
        instructions->push_back(inst);
    };

    [[nodiscard]] Value* analyze_expr(Facet const* expr_facet) {
        return prism::analyze_facet_as<Value>(*this, *this, scope, expr_facet);
    }

    void analyze(Facet const* facet) {
        if (!facet) return;
        visit(*facet, FN1(&, do_analyze(_1)));
    }

    void do_analyze(Facet const&) { PRISM_UNREACHABLE(); }

    void do_analyze(CompoundFacet const& facet) {
        auto* outer_scope = scope;
        auto* outer_instructions = instructions;
        utl::armed_scope_guard pop_scope = [&] {
            scope = outer_scope;
            instructions = outer_instructions;
        };
        scope = ctx.make_scope(outer_scope);
        std::vector<Instruction*> block_instructions;
        instructions = &block_instructions;
        for (auto* elem: facet.statements()->elems())
            analyze(elem);
        auto* block_type = [&]() -> Type const* {
            auto* yield_facet = facet.yieldFacet();
            if (!yield_facet) return ctx.get_void_type();
            auto* operand = analyze_expr(yield_facet);
            auto* yield_inst = ctx.make<YieldInst>(yield_facet, scope, operand);
            emit_instruction(yield_inst);
            return operand->type();
        }();
        auto* block_inst = ctx.make<BlockInst>(&facet, outer_scope,
                                               /* name: */ std::string{},
                                               ScopeArg(scope), block_type,
                                               std::move(block_instructions));
        pop_scope.execute();
        if (instructions) emit_instruction(block_inst);
    }

    void do_analyze(ExprStmtFacet const& stmt_facet) {
        (void)analyze_expr(stmt_facet.expr());
    }
};

} // namespace prism

void FuncAnaCtx::run() {
    if (auto* def_facet = dyncast<FuncDefFacet const*>(function.facet()))
        analyze(def_facet->body());
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
