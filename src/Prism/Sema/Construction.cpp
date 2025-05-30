#include "Prism/Sema/Construction.h"

#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>
#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/ExprAnalysis.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/SubContext.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using ranges::views::enumerate;
using ranges::views::transform;

// Declares all globally visible symbols in the given sources files to a module.
// Returns a list of all declared symbols
static std::vector<DeclSymbol*> construct_globals(
    SemaContext& ctx, DiagnosticEmitter& DE, Module& mod,
    std::span<SourceFilePair const> sources);

static size_t get_num_gen_params(GenParamListFacet const* gen_params) {
    return gen_params ? gen_params->elems().size() : 0;
}

namespace {

struct GlobalConstruction: AnalysisContext {
    std::vector<DeclSymbol*>& global_decls;

    template <std::derived_from<DeclSymbol> S, typename... Args>
        requires std::constructible_from<S, Args...>
    S* make(Args&&... args) {
        auto* s = ctx.make<S>(std::forward<Args>(args)...);
        global_decls.push_back(s);
        return s;
    }

    void construct(Facet const* facet, Scope* parent_scope) {
        if (!facet) return;
        visit(*facet, FN1(&, do_construct(_1, parent_scope)));
    }

    void do_construct(Facet const&, Scope*) { PRISM_UNREACHABLE(); }

    void do_construct(SourceFileFacet const& facet, Scope* parent_scope) {
        auto* file = ctx.make<SourceFile>(&facet, parent_scope,
                                          ScopeArg::make(ctx), *source_context);
        for (auto* decl: facet.decls())
            construct(decl, file->scope());
    }

    void do_construct(CompTypeDeclFacet const& facet, Scope* parent_scope) {
        std::string name = get_name(facet.name());
        size_t num_gen_params = get_num_gen_params(facet.genParams());
        if (!check_redefinition(parent_scope, facet, name)) return;
        auto* decl_symbol = [&]() -> DeclSymbol* {
            switch (facet.declarator().kind) {
            case TokenKind::Struct:
                return make<StructDef>(&facet, parent_scope, std::move(name),
                                       ScopeArg::make(ctx), num_gen_params);
                break;
            case TokenKind::Trait:
                return make<TraitDef>(&facet, parent_scope, std::move(name),
                                      ScopeArg::make(ctx), num_gen_params);
                break;
            default:
                PRISM_UNREACHABLE();
            }
        }();
        if (auto* body = facet.body())
            for (auto* child_decl_facet: body->elems())
                construct(child_decl_facet, decl_symbol->scope());
    }

    void do_construct(TypedefFacet const& facet, Scope* parent_scope) {
        std::string name = get_name(facet.nameFacet());
        size_t num_gen_params = get_num_gen_params(nullptr);
        if (!check_redefinition(parent_scope, facet, name)) return;
        make<TypeAliasDef>(&facet, parent_scope, std::move(name),
                           ScopeArg::make(ctx), num_gen_params);
    }

    void do_construct(TraitImplFacet const& facet, Scope* parent_scope) {
        size_t num_gen_params = get_num_gen_params(facet.genParams());
        auto* decl_symbol = make<TraitImplDef>(&facet, parent_scope,
                                               ScopeArg::make(ctx),
                                               num_gen_params);
        auto* def_facet = cast<TraitImplTypeFacet const*>(facet.definition());
        if (auto* body = def_facet->body())
            for (auto* child_decl_facet: body->elems())
                construct(child_decl_facet, decl_symbol->scope());
    }

    void do_construct(FuncDeclBaseFacet const& facet, Scope* parent_scope) {
        std::string name = get_name(facet.name());
        size_t num_gen_params = get_num_gen_params(facet.genParams());
        size_t num_args = facet.params() ? facet.params()->elems().size() : 0;
        if (!check_redefinition(parent_scope, facet, name,
                                /* is_function: */ true))
            return;
        make<FunctionDef>(&facet, parent_scope, std::move(name),
                          ScopeArg::make(ctx), num_gen_params, num_args);
    }

    void do_construct(VarDeclFacet const& facet, Scope* parent_scope) {
        std::string name = get_name(facet.name());
        if (!check_redefinition(parent_scope, facet, name)) return;
        make<BindingDef>(&facet, parent_scope, std::move(name), nullptr,
                         nullptr);
    }
};

} // namespace

static std::vector<DeclSymbol*> construct_globals(
    SemaContext& ctx, DiagnosticEmitter& DE, Module& mod,
    std::span<SourceFilePair const> sources) {
    std::vector<DeclSymbol*> result;
    for (auto [facet, source_context]: sources) {
        GlobalConstruction global_construction{ { ctx, DE, source_context },
                                                result };
        global_construction.construct(facet, mod.scope());
    }
    return result;
}

// Performs a DFS over the module containing global declarations to resolve name
// references, in particular
//  - type and trait specifiers in generic parameters
//  - type specifiers in function parameters
//  - type specifiers in member variables
static void resolve_global_names(SemaContext& ctx, DiagnosticEmitter& DE,
                                 std::span<DeclSymbol* const> decls);

namespace {

enum class NameResolutionState { Queued, InProgress, Done };

} // namespace

namespace prism {

struct NameResolution: AnalysisContext, FacetAnalysisDelegate {
    utl::hashmap<DeclSymbol const*, NameResolutionState> state_map;

    NameResolution(AnalysisContext analysis_context):
        AnalysisContext(analysis_context) {}

    // Delegate methods @{
    void emit_instruction(Instruction&) override { PRISM_UNREACHABLE(); }

    void encounter_callback(Symbol* symbol) override {
        auto* decl = dyncast<DeclSymbol*>(symbol);
        if (decl) resolve(*decl);
    }
    // @}

    void run(std::span<DeclSymbol* const> decls) {
        for (DeclSymbol* decl: decls)
            resolve(*decl);
    }

    NameResolutionState get_state(DeclSymbol const* decl) const {
        auto itr = state_map.find(decl);
        if (itr != state_map.end()) return itr->second;
        return NameResolutionState::Queued;
    }

    void set_state(DeclSymbol const* decl, NameResolutionState state) {
        state_map[decl] = state;
    }

    SubContext compute_sub_context(DeclSymbol* decl) {
        PRISM_ASSERT(decl);
        utl::stack<DeclSymbol*> stack;
        do {
            stack.push(decl);
            decl =
                dyncast<DeclSymbol*>(decl->parent_scope()->defining_symbol());
        } while (decl);
        SubContext sub_context;
        while (!stack.empty()) {
            auto* decl = stack.pop();
            PRISM_ASSERT(stack.empty() ||
                         get_state(decl) == NameResolutionState::Done);
            sub_context.push(decl->generic_params());
        }
        return sub_context;
    }

    Symbol* analyze_facet(Scope* scope, SubContext& sub_context,
                          Facet const* facet) {
        return prism::analyze_facet(*this, *this, sub_context, scope, facet);
    }

    template <std::derived_from<Symbol> S>
    S* analyze_facet_as(Scope* scope, SubContext& sub_context,
                        Facet const* facet) {
        return prism::analyze_facet_as<S>(*this, *this, sub_context, scope,
                                          facet);
    }

    void resolve(DeclSymbol& decl) {
        switch (get_state(&decl)) {
        case NameResolutionState::Queued:
            set_state(&decl, NameResolutionState::InProgress);
            visit(decl, FN1(&, do_resolve(_1)));
            set_state(&decl, NameResolutionState::Done);
            break;
        case NameResolutionState::InProgress:
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic for cycle
            break;
        case NameResolutionState::Done:
            break;
        }
    }

    Symbol* resolve_gen_param(GenParamDeclFacet const& facet,
                              SubContext& sub_context, size_t index,
                              DeclSymbol& decl) {
        auto* parent_scope = decl.scope();
        std::string name = get_name(facet.nameFacet());
        auto* req_symbol = analyze_facet(decl.parent_scope(), sub_context,
                                         facet.requirements());
        if (!check_redefinition(parent_scope, facet, name)) return nullptr;
        if (!req_symbol) return nullptr;
        uint32_t nesting_index =
            utl::narrow_cast<uint32_t>(sub_context.depth() - 1);
        if (auto* trait = dyncast<Trait*>(req_symbol))
            return ctx.get_gen_type_param(&facet, parent_scope, std::move(name),
                                          trait, index, nesting_index);
        if (auto* type = dyncast<Type*>(req_symbol))
            return ctx.get_gen_value_param(&facet, parent_scope,
                                           std::move(name), type, index,
                                           nesting_index);
        DE.emit<BadSymRef>(&facet, req_symbol, SymbolType::Trait);
        return nullptr;
    }

    SubContext resolve_gen_params(std::derived_from<DeclSymbol> auto& decl) {
        auto sub_context = compute_sub_context(&decl);
        decl._generic_nesting_depth =
            utl::narrow_cast<uint32_t>(sub_context.depth() - 1);
        auto* gen_params_facet = decl.facet()->genParams();
        if (!gen_params_facet) return sub_context;
        std::span gen_params = sub_context.top_level();
        PRISM_ASSERT(gen_params.size() == decl._generic_params.size());
        PRISM_ASSERT(gen_params.size() == gen_params_facet->elems().size());
        for (auto [index, facet]: gen_params_facet->elems() | enumerate)
            decl._generic_params[index] = gen_params[index] =
                resolve_gen_param(*facet, sub_context, index, decl);
        return sub_context;
    }

    void do_resolve(StructDef& struct_def) {
        auto sub_context = resolve_gen_params(struct_def);
        struct_def.set_canonical(
            ctx.get_struct_instantiation(sub_context, &struct_def));
    }

    void do_resolve(TypeAliasDef& type_alias_def) {
        auto sub_context = resolve_gen_params(type_alias_def);
        if (auto* aliased_facet = type_alias_def.facet()->initExpr())
            type_alias_def._aliased =
                analyze_facet_as<Type>(type_alias_def.scope(), sub_context,
                                       aliased_facet);
        type_alias_def.set_canonical(
            ctx.get_type_alias_instantiation(sub_context, &type_alias_def));
    }

    void do_resolve(TraitDef& trait_def) {
        auto sub_context = resolve_gen_params(trait_def);
        trait_def.set_canonical(
            ctx.get_trait_instantiation(sub_context, &trait_def));
    }

    void do_resolve(TraitImplDef& impl_def) {
        auto sub_context = resolve_gen_params(impl_def);
        auto* def_facet =
            cast<TraitImplTypeFacet const*>(impl_def.facet()->definition());
        impl_def._trait = analyze_facet_as<Trait>(impl_def.scope(), sub_context,
                                                  def_facet->traitDeclRef());
        impl_def._type =
            analyze_facet_as<Type>(impl_def.scope(), sub_context,
                                   def_facet->conformingTypename());
    }

    FunctionArgument* resolve_func_arg(SubContext& sub_context,
                                       ParamDeclFacet const* facet,
                                       size_t index, FunctionDef& func_def) {
        if (!facet) return nullptr;
        return visit(*facet, FN1(&, do_resolve_func_arg(sub_context, _1, index,
                                                        func_def)));
    }

    static PassingConvention get_passing_conv(TerminalFacet const* term) {
        if (!term) return PassingConvention::In;
        switch (term->token().kind) {
        case TokenKind::In:
            return PassingConvention::In;
        case TokenKind::Inout:
            return PassingConvention::Inout;
        case TokenKind::Sink:
            return PassingConvention::Sink;
        default:
            PRISM_UNREACHABLE();
        }
    }

    Type* get_this_type(Symbol* parent_symbol) {
        if (auto* struct_type = dyncast<StructDef*>(parent_symbol))
            return struct_type->canonical();
        if (auto* trait = dyncast<TraitDef*>(parent_symbol))
            return ctx.get_trait_this_type(trait->canonical());
        if (auto* impl = dyncast<TraitImplDef*>(parent_symbol))
            return impl->type();
        PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
        return nullptr;
    }

    FunctionArgument* do_resolve_func_arg(SubContext&,
                                          ThisParamDeclFacet const& facet,
                                          size_t index, FunctionDef& func_def) {
        Symbol* parent_symbol = func_def.parent_scope()->defining_symbol();
        auto* this_type = get_this_type(parent_symbol);
        if (!this_type) return nullptr;
        auto* function_scope = func_def.scope();
        auto passing_conv = get_passing_conv(facet.passingConventionFacet());
        if (index != 0) {
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic (this param invalid
                                   // position)
            return nullptr;
        }
        return ctx.make<FunctionArgument>(&facet, function_scope, "this",
                                          passing_conv, this_type,
                                          /* is_this: */ true);
    }

    FunctionArgument* do_resolve_func_arg(SubContext& sub_context,
                                          NamedParamDeclFacet const& facet,
                                          size_t, FunctionDef& func_def) {
        auto* function_scope = func_def.scope();
        std::string name = get_name(facet.nameFacet());
        auto passing_conv = get_passing_conv(facet.passingConventionFacet());
        auto* type = analyze_facet_as<Type>(function_scope, sub_context,
                                            facet.typespec());
        if (!check_redefinition(function_scope, facet, name)) return nullptr;
        return ctx.make<FunctionArgument>(&facet, function_scope,
                                          std::move(name), passing_conv, type,
                                          /* is_this: */ false);
    }

    void resolve_func_args(SubContext& sub_context, FunctionDef& func_def) {
        auto* params_facet = func_def.facet()->params();
        if (!params_facet) return;
        func_def._args =
            params_facet->elems() | enumerate |
            transform(FN1(&, resolve_func_arg(sub_context, _1.second, _1.first,
                                              func_def))) |
            ToSmallVector<>;
    }

    Type const* resolve_return_type(SubContext& sub_context,
                                    FunctionDef& func_def) {
        if (!func_def.facet()->retType()) return ctx.get_void_type();
        return analyze_facet_as<Type>(func_def.scope(), sub_context,
                                      func_def.facet()->retType());
    }

    void do_resolve(FunctionDef& func_def) {
        auto sub_context = resolve_gen_params(func_def);
        resolve_func_args(sub_context, func_def);
        func_def._return_type = resolve_return_type(sub_context, func_def);
        func_def.set_canonical(
            ctx.get_function_instantiation(sub_context, &func_def));
        auto* parent_scope = func_def.parent_scope();
        auto gen_signature = func_def.make_generic_signature();
        auto signature = func_def.make_signature();
        auto* existing = parent_scope->function_by_name_and_sig(func_def.name(),
                                                                gen_signature,
                                                                signature);
        if (existing) {
            DE.emit<FuncRedefinition>(func_def.facet(), &func_def, existing,
                                      parent_scope);
            return;
        }
        parent_scope->set_function_signature(std::move(gen_signature),
                                             std::move(signature), &func_def);
    }

    void do_resolve(BindingDef& binding) {
        auto sub_context = compute_sub_context(&binding);
        sub_context.pop(); // FIXME: sub_context should not get a stack entry
                           // for non-scoped declarations (?)
        auto* facet = binding.facet();
        if (auto* typespec_facet = facet->typespec())
            binding._type_spec = analyze_facet_as<Type>(binding.parent_scope(),
                                                        sub_context,
                                                        typespec_facet);
        else if (!facet->colonFacet())
            DE.emit<BindingMissingTypespec>(facet, binding.name());
        if (auto* init_expr = facet->initExpr()) {
            auto* init_value = analyze_facet_as<Value>(binding.parent_scope(),
                                                       sub_context, init_expr);
            auto* type = init_value ? init_value->type() : nullptr;
            auto* exp_type = binding.type_spec();
            if (type && exp_type && type != exp_type)
                DE.emit<BadOperandType>(init_expr, init_value, exp_type);
            binding._init = init_value;
            if (auto* const_init = dyncast<Constant*>(init_value))
                binding.set_canonical(const_init);
            else
                PRISM_UNIMPLEMENTED(); // TODO: create value class for this
                                       // purpose
        }
        else {
            DE.emit<BindingMissingInit>(binding.facet(), binding.name());
        }
    }

    void do_resolve(Library&) {}
    void do_resolve(GenTypeParam const&) {}
    void do_resolve(GenValueParam const&) {}
    void do_resolve(Type const&) {}
    void do_resolve(Trait const&) {}
};

} // namespace prism

static void resolve_global_names(SemaContext& ctx, DiagnosticEmitter& DE,
                                 std::span<DeclSymbol* const> decls) {
    NameResolution name_resolution{ { ctx, DE } };
    name_resolution.run(decls);
}

Module* prism::construct_sema_ir(SemaContext& ctx, DiagnosticEmitter& DE,
                                 std::span<SourceFilePair const> sources) {
    auto* mod = ctx.make_module();
    auto global_decls = construct_globals(ctx, DE, *mod, sources);
    resolve_global_names(ctx, DE, global_decls);
    return mod;
}
