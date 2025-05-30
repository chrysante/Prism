#include "Prism/Sema/ConformanceAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisContext.h"
#include "Prism/Sema/GenericMatching.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/SubContext.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

using ranges::views::iota;
using ranges::views::transform;
using ranges::views::zip;

namespace prism {

struct ConformanceAnalysis: AnalysisContext {
    void analyze(TraitDef& trait) { analyze_members(trait); }

    void analyze(TraitImplDef& impl) {
        if (!impl.trait()) return;
        analyze_members(impl);
        auto* trait = cast<TraitInst*>(impl.trait());
        utl::small_vector<DeclSymbol const*> missing_impls;
        auto* trait_scope = trait->definition()->scope();
        for (auto* sym: trait_scope->symbols() | csp::filter<DeclSymbol>)
            if (!impl.find_impl_for(sym)) missing_impls.push_back(sym);
        if (!missing_impls.empty())
            DE.emit<IncompleteTraitImpl>(impl.facet()->declaratorFacet(), &impl,
                                         missing_impls);
    }

    void analyze_members(DeclSymbol& decl) {
        utl::small_vector<FunctionDef*> func_defs;
        utl::small_vector<TypeAliasDef*> type_defs;
        for (auto* sym: decl.scope()->symbols() | csp::filter<DeclSymbol>) {
            if (auto* func_def = dyncast<FunctionDef*>(sym))
                func_defs.push_back(func_def);
            else if (auto* type_def = dyncast<TypeAliasDef*>(sym))
                type_defs.push_back(type_def);
            else
                PRISM_UNIMPLEMENTED();
        }
        for (auto* sym: type_defs)
            analyze_member(decl, *sym);
        for (auto* sym: func_defs)
            analyze_member(decl, *sym);
    }

    void analyze_member(DeclSymbol& decl, DeclSymbol& member) {
        return visit(decl, member, FN2(&, do_analyze_member(_1, _2)));
    }

    void do_analyze_member(DeclSymbol&, DeclSymbol&) { PRISM_UNREACHABLE(); }

    void do_analyze_member(TraitDef&, TypeAliasDef& type_def) {
        if (type_def.is_generic())
            DE.emit<GenericMemberInTrait>(type_def.facet()->genParams(),
                                          &type_def);
        if (type_def.aliased())
            PRISM_UNIMPLEMENTED(); // Don't support defaulted typedefs for now
    }

    void do_analyze_member(TraitDef&, FunctionDef& func_def) {
        if (func_def.is_generic())
            DE.emit<GenericMemberInTrait>(func_def.facet()->genParams(),
                                          &func_def);
        if (!func_def.has_this_parameter())
            DE.emit<NoThisInTraitFunction>(func_def.facet(), &func_def);
    }

    void do_analyze_member(TraitImplDef& impl, TypeAliasDef& type_def) {
        if (type_def.is_generic()) {
            PRISM_UNIMPLEMENTED();
            return;
        }
        auto* trait = cast<TraitInst*>(impl.trait());
        auto* def = trait->definition();
        auto* scope = def->scope();
        auto candidates = scope->symbols_by_name(type_def.name());
        TypeAliasDef* match = nullptr;
        for (auto* candidate: candidates) {
            auto* candidate_alias = dyncast<TypeAliasDef*>(candidate);
            if (!candidate_alias) PRISM_UNIMPLEMENTED();
            match = candidate_alias;
        }
        if (!match) {
            DE.emit<UnmatchedTraitImpl>(type_def.facet(), trait, &type_def);
            return;
        }
        impl._conformance_map.insert({ match, &type_def });
    }

    bool match_type(TraitImplDef const& trait_impl,
                    TraitThisType const* trait_this,
                    SubContext const& sub_context, Type const* in_trait,
                    Type const* in_impl) const {
        struct Matcher {
            TraitImplDef const& trait_impl;
            SubContext const& sub_context;
            TraitThisType const* trait_this;
            Type const* impl_this;

            bool null_fallback() const { return false; }

            bool exact_match() { return true; }

            bool structural(StructInst const& param, StructInst const& arg) {
                if (param.definition() != arg.definition()) return false;
                for (auto [p, a]: zip(param.generic_args(), arg.generic_args()))
                    if (!match_generic(*this, p, a)) return false;
                return true;
            }

            bool base_case(Symbol const& in_trait,
                           Symbol const& in_impl) const {
                if (auto* gen_param = as_gen_param_base(&in_trait))
                    return sub_context.resolve(*gen_param) ==
                           canonicalize(&in_impl);
                if (auto* alias = dyncast<TypeAliasInst const*>(&in_trait)) {
                    auto* impl_alias =
                        trait_impl.find_impl_for(alias->definition());
                    return impl_alias &&
                           canonicalize(impl_alias->canonical()) == &in_impl;
                }
                return &in_trait == trait_this && &in_impl == impl_this;
            }
        };

        return match_generic(Matcher{ trait_impl, sub_context, trait_this,
                                      trait_impl.type() },
                             in_trait, in_impl);
    }

    bool match_candidate(TraitImplDef const& trait_impl,
                         TraitThisType const* trait_this,
                         SubContext const& sub_context,
                         FunctionDef const& candidate,
                         FunctionDef const& impl_def) {
        if (candidate.num_arguments() != impl_def.num_arguments()) return false;
        // FIXME: do we really need this check?
        if (!candidate.has_this_parameter() || !impl_def.has_this_parameter())
            return false;
        for (auto [param, arg, index]:
             zip(candidate.arguments(), impl_def.arguments(), iota(0)))
        {
            if (!param || !arg)
                return false; // TODO: maybe 'indeterminate' instead of false?
            if (param->passing_convention() != arg->passing_convention())
                return false;
            if (!match_type(trait_impl, trait_this, sub_context, param->type(),
                            arg->type()))
                return false;
        }
        return match_type(trait_impl, trait_this, sub_context,
                          candidate.return_type(), impl_def.return_type());
    }

    void do_analyze_member(TraitImplDef& impl, FunctionDef& func_def) {
        if (func_def.is_generic()) {
            PRISM_UNIMPLEMENTED();
            return;
        }
        auto* trait = cast<TraitInst*>(impl.trait());
        auto* trait_this = ctx.get_trait_this_type(trait);
        auto* def = trait->definition();
        auto* scope = def->scope();
        auto candidates = scope->symbols_by_name(func_def.name());
        FunctionDef* match = nullptr;
        for (auto* candidate: candidates | transform(cast<FunctionDef*>)) {
            if (!match_candidate(impl, trait_this, trait->sub_context(),
                                 *candidate, func_def))
                continue;
            if (match) {
                PRISM_UNIMPLEMENTED();
                continue;
            }
            match = candidate;
        }
        if (!match) {
            DE.emit<UnmatchedTraitImpl>(func_def.facet(), trait, &func_def);
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
