#include "Prism/Sema2/FunctionAnalysis.h"

#include <utl/hashtable.hpp>
#include <utl/strcat.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/ExprAnalysis.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

static std::string render_unique_name(std::string_view non_unique,
                                      size_t counter) {
    if (non_unique.empty()) return std::to_string(counter);
    return utl::strcat(non_unique, ".", counter);
}

namespace prism {

struct FuncAnaCtx: AnalysisContext, InstructionEmitter {
    struct InstCounter {
        Instruction* last_encounter = nullptr;
        size_t num_encounters = 0;
    };

    FunctionDef& function;
    SubContext& sub_context;
    utl::hashmap<std::string, InstCounter> inst_name_map;

    FuncAnaCtx(SemaContext& ctx, DiagnosticEmitter& DE, FunctionDef& function,
               SubContext& sub_context):
        AnalysisContext{ ctx, DE, get_source_context(function.facet()) },
        function(function),
        sub_context(sub_context) {}

    void run();

    void set_unique_name(Instruction& inst, size_t counter) {
        inst.set_name(render_unique_name(inst.name(), counter));
    }

    void emit_instruction(Instruction& inst) final {
        if (inst.type() == ctx.get_void_type()) return;
        auto& counter = inst_name_map[inst.name()];
        if (inst.name().empty()) {
            set_unique_name(inst, counter.num_encounters);
        }
        else {
            if (counter.num_encounters == 1)
                set_unique_name(*counter.last_encounter,
                                counter.num_encounters - 1);
            if (counter.num_encounters >= 1)
                set_unique_name(inst, counter.num_encounters);
        }
        ++counter.num_encounters;
        counter.last_encounter = &inst;
    }
};

} // namespace prism

void FuncAnaCtx::run() {
    auto* def_facet = dyncast<FuncDefFacet const*>(function.facet());
    if (!def_facet) return;
    if (auto* body = dyncast<CompoundFacet const*>(def_facet->body()))
        function._body = analyze_facet_as<BlockInst>(*this, *this, sub_context,
                                                     function.scope(), body);
}

void prism::analyze_functions(SemaContext& ctx, DiagnosticEmitter& DE,
                              Module& mod) {
    SubContext sub_context;
    auto dfs = [&](auto& dfs, Scope* scope) -> void {
        if (!scope) return;
        for (auto* sym: scope->symbols() | ToSmallVector<>) {
            auto* gen_decl = dyncast<DeclSymbol*>(sym);
            if (gen_decl) sub_context.push(gen_decl->generic_params());
            if (auto* function = dyncast<FunctionDef*>(sym))
                FuncAnaCtx(ctx, DE, *function, sub_context).run();
            if (isa<SourceFile>(sym) || isa<StructDef>(sym) ||
                isa<TraitDef>(sym))
                dfs(dfs, sym->scope());
            if (gen_decl) sub_context.pop();
        }
    };
    dfs(dfs, mod.scope());
}
