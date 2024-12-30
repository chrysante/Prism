#include "Prism/Sema/Construction.h"

#include <string>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/function_view.hpp>
#include <utl/stack.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/ExprAnalysis.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaIssue.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::concat;
using ranges::views::drop;
using ranges::views::enumerate;
using ranges::views::transform;
using ranges::views::zip;

static void declareBuiltins(SemaContext& ctx, Scope* globalScope) {
    using enum Signedness;
#define SEMA_BUILTIN_TYPE(Name, Spelling, SymType, ...)                        \
    ctx.makeBuiltin<SymType>(BuiltinSymbol::Name, Spelling,                    \
                             globalScope __VA_OPT__(, ) __VA_ARGS__);
#include "Prism/Sema/Builtins.def"
    ctx.makeBuiltin<Trait>(BuiltinSymbol::Type, "type",
                           /* facet: */ nullptr, globalScope, nullptr,
                           std::nullopt);
}

static void makeCoreLibrary(SemaContext& ctx, Scope* globalScope) {
    auto& core = *ctx.make<Library>("core", globalScope);
    auto* coreScope = core.associatedScope();
    (void)coreScope;
}

namespace {

struct InstantiationBase: AnalysisBase {
    std::string getName(std::derived_from<DeclFacet> auto const& facet) const {
        PRISM_ASSERT(sourceContext);
        auto token = cast<TerminalFacet const&>(*facet.name()).token();
        PRISM_ASSERT(token.kind == TokenKind::Identifier);
        return std::string(sourceContext->getTokenStr(token));
    }
};

struct GloablDeclDeclare: InstantiationBase {
    Scope* globalScope;

    void run(std::span<SourceFilePair const> input) {
        ranges::for_each(input,
                         [&](auto& p) { declareFile(*p.facet, p.context); });
    }

    void declareChildren(Scope* scope, auto const& children) {
        ranges::for_each(children, FN1(&, declare(_1, scope)));
    }

    void declareFile(SourceFileFacet const& facet,
                     SourceContext const& sourceContext) {
        this->sourceContext = &sourceContext;
        auto* file = ctx.make<SourceFile>(sourceContext.filepath().string(),
                                          &facet, globalScope, sourceContext);
        declareChildren(file->associatedScope(), facet.decls());
    }

    Symbol* declare(Facet const* facet, Scope* scope) {
        if (!facet) return nullptr;
        return visit(*facet, FN1(&, doDeclare(_1, scope)));
    }

    Symbol* doDeclare(Facet const&, Scope const*) { return nullptr; }

    Symbol* doDeclare(FuncDefFacet const& facet, Scope* parent) {
        auto [context, scope] = makeGenContext(facet.genParams(), parent);
        if (facet.body() && isa<CompoundFacet>(facet.body()))
            return ctx.make<FunctionImpl>(getName(facet), &facet, parent, scope,
                                          std::move(context));
        if (context)
            PRISM_UNIMPLEMENTED(); // Can function declarations have generic
                                   // parameters?!
        return ctx.make<Function>(getName(facet), &facet, parent);
    }

    Symbol* doDeclare(CompTypeDeclFacet const& facet, Scope* parent) {
        auto [context, scope] = makeGenContext(facet.genParams(), parent);
        auto* typeOrTrait = [&]() -> Symbol* {
            switch (facet.declarator().kind) {
            case TokenKind::Struct:
                return ctx.make<StructType>(getName(facet), &facet, parent,
                                            scope, std::move(context));
            case TokenKind::Trait:
                return ctx.make<Trait>(getName(facet), &facet, parent, scope,
                                       std::move(context));
            default:
                PRISM_UNREACHABLE();
            }
        }();
        declareChildren(typeOrTrait->associatedScope(), facet.body()->elems());
        return typeOrTrait;
    }

    Symbol* doDeclare(TraitImplFacet const& facet, Scope* parent) {
        auto [context, scope] = makeGenContext(facet.genParams(), parent);
        auto* impl = ctx.make<TraitImpl>(&facet, parent, scope, nullptr,
                                         nullptr, std::move(context));
        declareChildren(impl->associatedScope(),
                        cast<TraitImplTypeFacet const*>(facet.definition())
                            ->body()
                            ->elems());
        return impl;
    }

    Symbol* doDeclare(GenParamDeclFacet const& facet, Scope* parent) {
        auto* trait =
            analyzeFacetAs<Trait>(*this, parent, facet.requirements());
        auto name = sourceContext->getTokenStr(facet.name());
        return ctx.make<GenericTypeParam>(std::string(name), &facet, parent,
                                          trait);
    }

    struct GenCtxAnaResult {
        std::optional<GenericContext> context;
        Scope* scope;
    };

    /// Creates a generic context if \p genParams is not null. In this case a
    /// scope for the generic symbol is created. Otherwise `{ std::nullopt,
    /// nullptr }` is returned and the respective symbol creates its own scope.
    GenCtxAnaResult makeGenContext(GenParamListFacet const* genParams,
                                   Scope* parent) {
        if (!genParams) return {};
        auto* scope = ctx.make<Scope>(parent);
        return { GenericContext(genParams->children() |
                                transform(FN1(&, declare(_1, scope))) |
                                ToSmallVector<>),
                 scope };
    }
};

} // namespace

static void declareGlobals(SemaContext& ctx, IssueHandler& iss,
                           Scope* globalScope,
                           std::span<SourceFilePair const> input) {
    GloablDeclDeclare{ { ctx, iss }, globalScope }.run(input);
}

namespace prism {

struct GlobalNameResolver: InstantiationBase {
    DependencyGraph& dependencies;

    DependencyNode* getNode(Symbol& sym) { return dependencies.getNode(sym); }

    void addDependency(DependencyNode* node, Symbol* dependsOn) {
        if (dependsOn && !isBuiltinSymbol(*dependsOn)) {
            node->addDependency(getNode(*dependsOn));
        }
    }

    void addDependency(Symbol& symbol, Symbol* dependsOn) {
        addDependency(getNode(symbol), dependsOn);
    }

    void resolve(Symbol* symbol) {
        if (!symbol) return;
        visit(*symbol, [this](auto& symbol) { doResolve(symbol); });
    }

    void doResolve(Symbol&) {}

    void declareGlobalVar(Scope* parent, VarDeclFacet const& facet) {
        if (!facet.typespec()) {
            PRISM_UNIMPLEMENTED();
        }
        auto* type = analyzeFacetAs<ValueType>(*this, parent, facet.typespec());
        auto* var = ctx.make<Variable>(getName(facet), &facet, parent,
                                       QualType{ type, {} });
        addDependency(*var, type);
    }

    void doResolve(SourceFile& sourceFile) {
        sourceContext = &sourceFile.sourceContext();
        auto globals = sourceFile.facet()->decls() | csp::filter<VarDeclFacet>;
        for (auto* decl: globals)
            declareGlobalVar(sourceFile.associatedScope(), *decl);
        resolveChildren(sourceFile);
    }

    void doResolve(TraitImpl& impl) {
        auto* def = cast<TraitImplTypeFacet const*>(impl.facet()->definition());
        impl._trait = analyzeFacetAs<Trait>(*this, impl.parentScope(),
                                            def->traitDeclRef());
        impl._conf = analyzeFacetAs<CompositeType>(*this, impl.parentScope(),
                                                   def->conformingTypename());
        auto* node = getNode(impl);
        addDependency(node, impl._trait);
        addDependency(node, impl._conf);
        resolveChildren(impl);
    }

    Symbol* declareBase(Scope* scope, BaseDeclFacet const& decl) {
        auto* base = analyzeFacet(*this, scope, decl.type());
        if (!base) return nullptr;
        if (auto* type = dyncast<UserType*>(base)) {
            auto* baseclass = ctx.make<BaseClass>(&decl, scope, type);
            addDependency(*baseclass, type);
            return baseclass;
        }
        if (auto* trait = dyncast<Trait*>(base)) {
            auto* basetrait = ctx.make<BaseTrait>(&decl, scope, trait);
            addDependency(*basetrait, trait);
            return basetrait;
        }
        PRISM_UNIMPLEMENTED();
    }

    MemberVar* declareMemberVar(Scope* scope, VarDeclFacet const& decl) {
        auto* type = analyzeFacetAs<ValueType>(*this, scope, decl.typespec());
        auto* var = ctx.make<MemberVar>(getName(decl), &decl, scope, type);
        addDependency(*var, type);
        return var;
    }

    void declareMembers(Symbol& typeOrTrait,
                        utl::function_view<void(Symbol*)> verify) {
        auto* scope = typeOrTrait.associatedScope();
        auto* node = getNode(typeOrTrait);
        auto* facet = cast<CompTypeDeclFacet const*>(typeOrTrait.facet());
        if (!facet) return;
        if (auto* bases = facet->bases()) {
            for (auto* decl: bases->elems()) {
                auto* base = declareBase(scope, *decl);
                addDependency(node, base);
                verify(base);
            }
        }
        if (auto* body = facet->body()) {
            for (auto* decl: body->elems() | csp::filter<VarDeclFacet>) {
                auto* var = declareMemberVar(scope, *decl);
                addDependency(node, var);
                verify(var);
            }
        }
    }

    void doResolve(CompositeType& type) {
        declareMembers(type, [&](Symbol* sym) {
            if (auto* basetrait = dyncast<BaseTrait*>(sym))
                type._baseTraits.push_back(basetrait);
            else if (auto* baseclass = dyncast<BaseClass*>(sym))
                type._bases.push_back(baseclass);
            else if (auto* memvar = dyncast<MemberVar*>(sym))
                type._memvars.push_back(memvar);
        });
        resolveChildren(type);
    }

    void doResolve(Trait& trait) {
        declareMembers(trait, [&](Symbol* sym) {
            if (auto* baseclass = dyncast<BaseClass*>(sym))
                PRISM_UNIMPLEMENTED(); // Error
            else if (auto* memvar = dyncast<MemberVar*>(sym))
                PRISM_UNIMPLEMENTED(); // Error
        });
        resolveChildren(trait);
    }

    FuncParam* analyzeParam(Function& func, ParamDeclFacet const* facet,
                            size_t index) {
        if (!facet) return nullptr;
        return visit(*facet, [&](auto& facet) {
            return analyzeParamImpl(func, facet, index);
        });
    }

    FuncParam* analyzeParamImpl(Function& func,
                                NamedParamDeclFacet const& param,
                                size_t /* index */) {
        auto* type =
            analyzeFacetAs<Type>(*this, func.parentScope(), param.typespec());
        auto name = sourceContext->getTokenStr(param.name());
        return ctx.make<FuncParam>(std::string(name), &param, type,
                                   FuncParam::Options{ .hasMut = false,
                                                       .isThis = false });
    }

    FuncParam* analyzeParamImpl(Function& func, ThisParamDeclFacet const& param,
                                size_t index) {
        if (index != 0) {
            iss.push<ThisParamBadPosition>(sourceContext, &param);
        }
        Mutability mut = Mutability::Const;
        bool dyn = false, ref = false;
        auto* typeFacet = param.spec();
        while (!isa<TerminalFacet>(typeFacet)) {
            auto* prefix = cast<PrefixFacet const*>(typeFacet);
            typeFacet = prefix->operand();
            using enum TokenKind;
            switch (prefix->operation().kind) {
            case Mut:
                mut = Mutability::Mut;
                break;
            case Dyn:
                dyn = true;
                break;
            case Ampersand:
                ref = true;
                break;
            default:
                PRISM_UNREACHABLE();
            }
        }
        PRISM_ASSERT(cast<TerminalFacet const*>(typeFacet)->token().kind ==
                     TokenKind::This);
        auto* parent = func.parentScope()->assocSymbol();
        auto* thisType = [&]() -> ValueType const* {
            if (auto* userType = dyncast<UserType*>(parent)) return userType;
            if (auto* trait = dyncast<Trait*>(parent))
                return ctx.getDynTraitType(trait);
            if (auto* traitImpl = dyncast<TraitImpl*>(parent))
                return traitImpl->conformingType();
            PRISM_UNIMPLEMENTED();
        }();
        if (ref) {
            auto* type = ctx.getRefType({ thisType, mut });
            return ctx.make<FuncParam>("this", &param, type,
                                       FuncParam::Options{ .hasMut = false,
                                                           .isThis = true });
        }
        else {
            return ctx.make<FuncParam>("this", &param, thisType,
                                       FuncParam::Options{
                                           .hasMut = mut == Mutability::Mut,
                                           .isThis = true });
        }
    }

    void doResolve(Function& func) {
        if (auto* paramDecls = func.facet()->params())
            func._params =
                paramDecls->elems() | enumerate |
                transform(FN1(&, analyzeParam(func, _1.second, _1.first))) |
                ToSmallVector<>;
        auto* retType = [&]() -> Type const* {
            if (auto* retFacet = func.facet()->retType())
                return analyzeFacetAs<Type>(*this, func.parentScope(),
                                            retFacet);
            return ctx.getVoid();
        }();
        func._sig = FuncSig::Compute(retType, func.params());
    }

    void resolveChildren(std::span<Symbol* const> symbols) {
        for (auto* symbol: symbols)
            resolve(symbol);
    }

    void resolveChildren(auto& symbol) {
        resolveChildren(symbol.associatedScope()->symbols());
    }
};

} // namespace prism

static DependencyGraph resolveGlobalNames(MonotonicBufferResource& resource,
                                          SemaContext& ctx, IssueHandler& iss,
                                          Scope* globalScope) {
    DependencyGraph dependencies(resource);
    GlobalNameResolver{ { ctx, iss }, dependencies }.resolveChildren(
        globalScope->symbols());
    return dependencies;
}

namespace prism {

struct InstantiationContext: AnalysisBase {
    void instantiate(Symbol&) {}

    using LayoutAccumulator =
        utl::function_view<TypeLayout(TypeLayout, TypeLayout)>;

    static size_t align(size_t size, size_t al) {
        if (al == 0) {
            PRISM_ASSERT(size == 0, "Align == 0 must imply size == 0");
            return size;
        }
        if (size % al == 0) return size;
        return size + al - size % al;
    }

    TypeLayout computeLayout(CompositeType const& type, LayoutAccumulator acc) {
        TypeLayout layout = { 0, 0, 0 };
        auto members =
            concat(type.baseClasses() | transform(cast<MemberSymbol const*>),
                   type.memberVars());
        for (auto* member: members) {
            // For types with invalid members we report poison
            auto* memtype = member->type();
            if (!memtype || !memtype->layout().isComplete())
                return TypeLayout::Poison;
            layout = acc(layout, memtype->layout());
        }
        return { layout.size(), align(layout.size(), layout.alignment()),
                 layout.alignment() };
    }

    void instantiate(StructType& type) {
        auto layout = computeLayout(type, [](TypeLayout curr, TypeLayout next) {
            size_t size = align(curr.size(), next.alignment()) + next.size();
            size_t align = std::max(curr.alignment(), next.alignment());
            return TypeLayout{ size, size, align };
        });
        type.setLayout(layout);
    }
};

} // namespace prism

static void instantiateSymbol(SemaContext& ctx, IssueHandler& iss,
                              Symbol* symbol) {
    InstantiationContext instctx{ ctx, iss };
    visit(*symbol, [&](auto& symbol) { instctx.instantiate(symbol); });
}

ConstructionResult prism::constructTarget(
    MonotonicBufferResource& resource, SemaContext& ctx, IssueHandler& iss,
    std::span<SourceFilePair const> input) {
    auto* target = ctx.make<Target>(ctx, "TARGET");
    declareBuiltins(ctx, target->associatedScope());
    makeCoreLibrary(ctx, target->associatedScope());
    declareGlobals(ctx, iss, target->associatedScope(), input);
    auto dependencies =
        resolveGlobalNames(resource, ctx, iss, target->associatedScope());
    dependencies.topsort();
    if (dependencies.hasCycle()) {
        iss.push<TypeDefCycle>(dependencies.getCycle());
        return ConstructionResult::Fatal(target);
    }
    for (auto* symbol: dependencies.getTopoOrder() | ranges::views::reverse)
        instantiateSymbol(ctx, iss, symbol);
    return { target, std::move(dependencies) };
}
