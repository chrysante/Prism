#include "Prism/Sema2/ConformanceAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/SubContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::transform;

namespace {

struct ConfAnaContext: AnalysisContext {
    void analyze(TraitDef&) {}

    void analyze(TraitImplDef& impl) {
        auto* scope = impl.scope();
        for (auto* sym: scope->symbols())
            analyze_impl_member(impl, *sym);
    }

    void analyze_impl_member(TraitImplDef& impl, Symbol& symbol) {
        return visit(symbol, FN1(&, do_analyze_impl_member(impl, _1)));
    }

    void do_analyze_impl_member(TraitImplDef&, Symbol&) {
        PRISM_UNIMPLEMENTED();
    }

    bool compare_type(SubContext const& sub_context, Type const* impl_site,
                      Type const* trait_site) {
        PRISM_UNIMPLEMENTED();
    }

    void do_analyze_impl_member(TraitImplDef& impl, FunctionDef& func_def) {
        PRISM_UNIMPLEMENTED();
#if 0
        auto* trait = cast<TraitInst*>(impl.trait());
        auto candidates = trait->scope()->symbols_by_name(func_def.name());
        if (candidates.empty() || !ranges::all_of(candidates, isa<FunctionDef>)) {
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
            return;
        }
        SubContext trait_sub_context;
        trait_sub_context.push(trait->definition(), trait->generic_args());
        for (auto* candidate: candidates | transform(cast<FunctionDef*>)) {
            if (candidate->num_arguments() != func_def.num_arguments())
                continue;
            
        }
#endif
    }
};

} // namespace

void prism::analyze_trait_conformances(SemaContext& ctx, DiagnosticEmitter& DE,
                                       Module& mod) {
    utl::small_vector<TraitDef*> traits;
    utl::small_vector<TraitImplDef*> impls;
    auto dfs = [&](auto& dfs, Symbol& sym) {
        if (auto* trait = dyncast<TraitDef*>(&sym))
            traits.push_back(trait);
        else if (auto* impl = dyncast<TraitImplDef*>(&sym))
            impls.push_back(impl);
        if (!sym.scope() || (!isa<Module>(sym) && !isa<SourceFile>(sym)))
            return;
        for (auto* child_sym: sym.scope()->symbols())
            dfs(dfs, *child_sym);
    };
    dfs(dfs, mod);
    ConfAnaContext ana_context = { ctx, DE };
    for (auto* trait: traits)
        ana_context.analyze(*trait);
    for (auto* impl: impls)
        ana_context.analyze(*impl);
}
