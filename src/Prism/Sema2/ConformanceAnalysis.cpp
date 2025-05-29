#include "Prism/Sema2/ConformanceAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/GenericMatching.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/SubContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::iota;
using ranges::views::transform;
using ranges::views::zip;

namespace prism {

struct ConformanceAnalysis: AnalysisContext {
    void analyze(TraitDef& trait) {
        auto* scope = trait.scope();
        for (auto* sym: scope->symbols() | csp::filter<DeclSymbol>)
            analyze_member(trait, *sym);
    }

    void analyze(TraitImplDef& impl) {
        auto* scope = impl.scope();
        for (auto* sym: scope->symbols() | csp::filter<DeclSymbol>)
            analyze_member(impl, *sym);
    }

    void analyze_member(DeclSymbol& decl, DeclSymbol& member) {
        return visit(decl, member, FN2(&, do_analyze_member(_1, _2)));
    }

    void do_analyze_member(DeclSymbol&, DeclSymbol&) { PRISM_UNREACHABLE(); }

    void do_analyze_member(TraitDef& trait, FunctionDef& func_def) {
        if (func_def.is_generic()) {
            PRISM_UNIMPLEMENTED();
            return;
        }
        if (!func_def.has_this_parameter()) {
            PRISM_UNIMPLEMENTED();
            return;
        }
    }

    bool match_candidate(SubContext const& sub_context,
                         FunctionDef const& candidate, FunctionDef& impl_def) {
        if (candidate.num_arguments() != impl_def.num_arguments()) return false;
        auto match_callback = FN2(&, sub_context.resolve(_1) == &_2);
        if (!candidate.has_this_parameter() || !impl_def.has_this_parameter())
            return false;
        for (auto [param, arg, index]:
             zip(candidate.arguments(), impl_def.arguments(), iota(0)))
        {
            if (!param || !arg)
                return false; // TODO: maybe 'indeterminate' instead of false?
            if (param->passing_convention() != arg->passing_convention())
                return false;
            if (index > 0) {
                bool type_match = match_generic(param, arg, match_callback);
                if (!type_match) return false;
            }
        }
        return match_generic(candidate.return_type(), impl_def.return_type(),
                             match_callback);
    }

    void do_analyze_member(TraitImplDef& impl, FunctionDef& func_def) {
        if (func_def.is_generic()) {
            PRISM_UNIMPLEMENTED();
            return;
        }
        auto* trait = cast<TraitInst*>(impl.trait());
        auto* def = trait->definition();
        auto* scope = def->scope();
        auto candidates = scope->symbols_by_name(func_def.name());
        FunctionDef* match = nullptr;
        for (auto* candidate: candidates | transform(cast<FunctionDef*>)) {
            if (!match_candidate(trait->sub_context(), *candidate, func_def))
                continue;
            if (match) {
                PRISM_UNIMPLEMENTED();
                continue;
            }
            match = candidate;
        }
        if (!match) {
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
            return;
        }
        impl._conformance_map.insert({ match, &func_def });
    }
};

} // namespace prism

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
    ConformanceAnalysis ana_context = { ctx, DE };
    for (auto* trait: traits)
        ana_context.analyze(*trait);
    for (auto* impl: impls)
        ana_context.analyze(*impl);
}
