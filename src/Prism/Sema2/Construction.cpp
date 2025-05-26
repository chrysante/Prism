#include "Prism/Sema2/Construction.h"

#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/ExprAnalysis.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using ranges::views::transform;

static std::string get_name(Facet const* name_facet,
                            SourceContext const& source_context) {
    if (!name_facet) return {};
    auto* term = cast<TerminalFacet const*>(name_facet);
    return std::string(source_context.getTokenStr(term->token()));
}

// Declares all globally visible symbols in the given sources files to a module
static void construct_globals(SemaContext& ctx, DiagnosticEmitter& DE,
                              Module& mod,
                              std::span<SourceFilePair const> sources);

static size_t get_num_gen_params(GenParamListFacet const* gen_params) {
    return gen_params ? gen_params->elems().size() : 0;
}

namespace {

struct GlobalConstruction {
    SemaContext& ctx;
    DiagnosticEmitter& DE;
    SourceContext const& source_context;

    void construct(Facet const* facet, Scope* parent_scope) {
        if (!facet) return;
        visit(*facet, FN1(&, do_construct(_1, parent_scope)));
    }

    void do_construct(Facet const&, Scope*) { PRISM_UNREACHABLE(); }

    void do_construct(SourceFileFacet const& facet, Scope* parent_scope) {
        auto* file = ctx.make<SourceFile>(&facet, parent_scope,
                                          ScopeArg::make(ctx), source_context);
        for (auto* decl: facet.decls())
            construct(decl, file->scope());
    }

    void do_construct(CompTypeDeclFacet const& facet, Scope* parent_scope) {
        std::string name = get_name(facet.name(), source_context);
        size_t num_gen_params = get_num_gen_params(facet.genParams());
        auto* decl_symbol = [&]() -> DeclSymbol* {
            switch (facet.declarator().kind) {
            case TokenKind::Struct:
                return ctx.make<StructDef>(&facet, parent_scope,
                                           std::move(name), ScopeArg::make(ctx),
                                           num_gen_params);
                break;
            case TokenKind::Trait:
                return ctx.make<TraitDef>(&facet, parent_scope, std::move(name),
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

    void do_construct(FuncDeclBaseFacet const& facet, Scope* parent_scope) {
        std::string name = get_name(facet.name(), source_context);
        size_t num_gen_params = get_num_gen_params(facet.genParams());
        size_t num_args = facet.params() ? facet.params()->elems().size() : 0;
        ctx.make<FunctionDef>(&facet, parent_scope, std::move(name),
                              ScopeArg::make(ctx), num_gen_params, num_args);
    }
};

} // namespace

static void construct_globals(SemaContext& ctx, DiagnosticEmitter& DE,
                              Module& mod,
                              std::span<SourceFilePair const> sources) {
    for (auto [facet, source_context]: sources) {
        GlobalConstruction global_construction{ ctx, DE, *source_context };
        global_construction.construct(facet, mod.scope());
    }
}

// Performs a DFS over the module containing global declarations to resolve name
// references, in particular
//  - type and trait specifiers in generic parameters
//  - type specifiers in function parameters
//  - type specifiers in member variables
static void resolve_global_names(SemaContext& ctx, DiagnosticEmitter& DE,
                                 Module& mod);

namespace prism {

struct TrappingInstEmitter final: InstructionEmitter {
    void emit_instruction(Instruction&) override { PRISM_UNREACHABLE(); }
};

struct NameResolution: AnalysisContext {
    Symbol* analyze_facet(Scope* scope, Facet const* facet) {
        TrappingInstEmitter inst_emitter;
        return prism::analyze_facet(*this, inst_emitter, scope, facet);
    }

    template <std::derived_from<Symbol> S>
    S* analyze_facet_as(Scope* scope, Facet const* facet) {
        TrappingInstEmitter inst_emitter;
        return prism::analyze_facet_as<S>(*this, inst_emitter, scope, facet);
    }

    void resolve(Symbol& symbol) { visit(symbol, FN1(&, do_resolve(_1))); }

    void do_resolve(Symbol&) { PRISM_UNREACHABLE(); }

    // DFS helper
    void resolve_children(Scope* scope) {
        for (auto* child_sym: scope->symbols() | ToSmallVector<>)
            resolve(*child_sym);
    }

    void do_resolve(Module& mod) { resolve_children(mod.scope()); }

    void do_resolve(SourceFile& source_file) {
        source_context = &source_file.source_context();
        resolve_children(source_file.scope());
    }

    Symbol* resolve_gen_param(GenParamDeclFacet const& facet,
                              DeclSymbol& decl) {
        std::string name = get_name(facet.nameFacet(), *source_context);
        auto* req_symbol =
            analyze_facet(decl.parent_scope(), facet.requirements());
        if (auto* trait = dyncast<Trait*>(req_symbol))
            return ctx.make<GenTypeParam>(&facet, decl.scope(), std::move(name),
                                          trait);
        if (auto* type = dyncast<Type*>(req_symbol))
            return ctx.make<GenValueParam>(&facet, decl.scope(),
                                           std::move(name), type);
        PRISM_UNIMPLEMENTED();
    }

    utl::small_vector<Symbol*> resolve_gen_params(
        std::derived_from<DeclSymbol> auto& decl) {
        auto* gen_params_facet = decl.facet()->genParams();
        if (!gen_params_facet) return {};
        return gen_params_facet->elems() |
               transform(FN1(&, resolve_gen_param(*_1, decl))) |
               ToSmallVector<>;
    }

    void do_resolve(StructDef& struct_def) {
        auto gen_params = resolve_gen_params(struct_def);
        PRISM_ASSERT(gen_params.size() == struct_def._generic_params.size());
        if (!gen_params.empty())
            struct_def._generic_params = std::move(gen_params);
        resolve_children(struct_def.scope());
    }

    void do_resolve(TraitDef& trait_def) {
        auto gen_params = resolve_gen_params(trait_def);
        PRISM_ASSERT(gen_params.size() == trait_def._generic_params.size());
        if (!gen_params.empty())
            trait_def._generic_params = std::move(gen_params);
        resolve_children(trait_def.scope());
    }

    FunctionArgument* resolve_func_arg(ParamDeclFacet const* facet,
                                       FunctionDef& func_def) {
        if (!facet) return nullptr;
        return visit(*facet, FN1(&, do_resolve_func_arg(_1, func_def)));
    }

    FunctionArgument* do_resolve_func_arg(ThisParamDeclFacet const& facet,
                                          FunctionDef& func_def) {
        PRISM_UNIMPLEMENTED();
    }

    FunctionArgument* do_resolve_func_arg(NamedParamDeclFacet const& facet,
                                          FunctionDef& func_def) {
        std::string name = get_name(facet.nameFacet(), *source_context);
        PassingConvention passing_conv = [&] {
            if (!facet.passingConventionFacet()) return PassingConvention::In;
            switch (facet.passingConvention().kind) {
            case TokenKind::In:
                return PassingConvention::In;
            case TokenKind::Inout:
                return PassingConvention::Inout;
            case TokenKind::Sink:
                return PassingConvention::Sink;
            default:
                PRISM_UNREACHABLE();
            }
        }();
        Type const* type =
            analyze_facet_as<Type>(func_def.scope(), facet.typespec());
        return ctx.make<FunctionArgument>(&facet, func_def.scope(),
                                          std::move(name), passing_conv, type);
    }

    utl::small_vector<FunctionArgument*> resolve_func_args(
        FunctionDef& func_def) {
        auto* params_facet = func_def.facet()->params();
        if (!params_facet) return {};
        return params_facet->elems() |
               transform(FN1(&, resolve_func_arg(_1, func_def))) |
               ToSmallVector<>;
    }

    Type const* resolve_return_type(FunctionDef& func_def) {
        if (!func_def.facet()->retType()) return ctx.get_void_type();
        return analyze_facet_as<Type>(func_def.scope(),
                                      func_def.facet()->retType());
    }

    void do_resolve(FunctionDef& func_def) {
        auto gen_params = resolve_gen_params(func_def);
        PRISM_ASSERT(gen_params.size() == func_def._generic_params.size());
        if (!gen_params.empty())
            func_def._generic_params = std::move(gen_params);
        func_def._args = resolve_func_args(func_def);
        func_def._return_type = resolve_return_type(func_def);
        if (gen_params.empty())
            func_def.set_canonical(
                ctx.get_function_instantiation(&func_def, {}));
    }

    void do_resolve(Library&) {}
    void do_resolve(GenTypeParam const&) {}
    void do_resolve(GenValueParam const&) {}
    void do_resolve(Type const&) {}
    void do_resolve(Trait const&) {}
};

} // namespace prism

static void resolve_global_names(SemaContext& ctx, DiagnosticEmitter& DE,
                                 Module& mod) {
    NameResolution{ ctx, DE }.resolve(mod);
}

Module* prism::construct_sema_ir(SemaContext& ctx, DiagnosticEmitter& DE,
                                 std::span<SourceFilePair const> sources) {
    auto* mod = ctx.make_module();
    construct_globals(ctx, DE, *mod, sources);
    resolve_global_names(ctx, DE, *mod);
    return mod;
}
