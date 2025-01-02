#include "Prism/Sema/Construction.h"

#include <string>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/function_view.hpp>
#include <utl/stack.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/DiagnosticHandler.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/ExprAnalysis.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
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
                           /* facet: */ nullptr, globalScope);
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

// MARK: - Initial declaration of global names

struct GlobalDeclDeclare: InstantiationBase {
    Scope* globalScope;

    void run(std::span<SourceFilePair const> input);
    void declareChildren(Scope* scope, auto const& children);
    void declareFile(SourceFileFacet const& facet,
                     SourceContext const& sourceContext);
    Symbol* declare(Facet const* facet, Scope* scope);
    Symbol* doDeclare(Facet const&, Scope const*) { return nullptr; }
    Symbol* doDeclare(FuncDefFacet const& facet, Scope* parent);
    Symbol* doDeclareGen(FuncDefFacet const& facet, Scope* parent);
    Symbol* doDeclare(CompTypeDeclFacet const& facet, Scope* parent);
    Symbol* doDeclareGen(CompTypeDeclFacet const& facet, Scope* parent);
    Symbol* doDeclare(TraitImplFacet const& facet, Scope* parent);
    Symbol* doDeclareGen(TraitImplFacet const& facet, Scope* parent);
    Symbol* doDeclare(GenParamDeclFacet const& facet, Scope* parent);

    struct GenCtxAnaResult {
        Scope* scope;
        utl::small_vector<Symbol*> genParams;
    };

    /// Creates a generic context if \p genParams is not null. In this case a
    /// scope for the generic symbol is created. Otherwise
    /// `{ std::nullopt, nullptr }` is returned and the respective symbol
    /// creates its own scope.
    GenCtxAnaResult makeGenScope(GenParamListFacet const* genParams,
                                 Scope* parent);
};

} // namespace

void GlobalDeclDeclare::run(std::span<SourceFilePair const> input) {
    ranges::for_each(input, FN1(&, declareFile(*_1.facet, *_1.context)));
}

void GlobalDeclDeclare::declareChildren(Scope* scope, auto const& children) {
    ranges::for_each(children, FN1(&, declare(_1, scope)));
}

void GlobalDeclDeclare::declareFile(SourceFileFacet const& facet,
                                    SourceContext const& sourceContext) {
    this->sourceContext = &sourceContext;
    auto* file = ctx.make<SourceFile>(sourceContext.filepath().string(), &facet,
                                      globalScope, sourceContext);
    declareChildren(file->associatedScope(), facet.decls());
}

Symbol* GlobalDeclDeclare::declare(Facet const* facet, Scope* scope) {
    if (!facet) return nullptr;
    return visit(*facet, FN1(&, doDeclare(_1, scope)));
}

Symbol* GlobalDeclDeclare::doDeclare(FuncDefFacet const& facet, Scope* parent) {
    if (facet.genParams()) return doDeclareGen(facet, parent);
    if (facet.body() && isa<CompoundFacet>(facet.body()))
        return ctx.make<FunctionImpl>(getName(facet), &facet, parent);
    return ctx.make<Function>(getName(facet), &facet, parent);
}

Symbol* GlobalDeclDeclare::doDeclareGen(FuncDefFacet const& facet,
                                        Scope* parent) {
    PRISM_EXPECT(facet.genParams());
    auto [scope, genParams] = makeGenScope(facet.genParams(), parent);
    return ctx.make<GenFuncImpl>(getName(facet), &facet, parent, scope,
                                 std::move(genParams));
}

Symbol* GlobalDeclDeclare::doDeclare(CompTypeDeclFacet const& facet,
                                     Scope* parent) {
    if (facet.genParams()) return doDeclareGen(facet, parent);
    auto* typeOrTrait = [&]() -> Symbol* {
        switch (facet.declarator().kind) {
        case TokenKind::Struct:
            return ctx.make<StructType>(getName(facet), &facet, parent);
        case TokenKind::Trait:
            return ctx.make<Trait>(getName(facet), &facet, parent);
        default:
            PRISM_UNREACHABLE();
        }
    }();
    declareChildren(typeOrTrait->associatedScope(), facet.body()->elems());
    return typeOrTrait;
}

Symbol* GlobalDeclDeclare::doDeclareGen(CompTypeDeclFacet const& facet,
                                        Scope* parent) {
    auto [scope, genParams] = makeGenScope(facet.genParams(), parent);
    auto* typeOrTrait = [&]() -> Symbol* {
        switch (facet.declarator().kind) {
        case TokenKind::Struct:
            return ctx.make<GenStructType>(getName(facet), &facet, parent,
                                           scope, std::move(genParams));
        case TokenKind::Trait:
            return ctx.make<GenTrait>(getName(facet), &facet, parent, scope,
                                      std::move(genParams));
        default:
            PRISM_UNREACHABLE();
        }
    }();
    declareChildren(typeOrTrait->associatedScope(), facet.body()->elems());
    return typeOrTrait;
}

Symbol* GlobalDeclDeclare::doDeclare(TraitImplFacet const& facet,
                                     Scope* parent) {
    if (facet.genParams()) return doDeclareGen(facet, parent);
    auto* impl = ctx.make<TraitImpl>(&facet, parent);
    auto* implFacet = cast<TraitImplTypeFacet const*>(facet.definition());
    PRISM_ASSERT(implFacet->body());
    declareChildren(impl->associatedScope(), implFacet->body()->elems());
    return impl;
}

Symbol* GlobalDeclDeclare::doDeclareGen(TraitImplFacet const& facet,
                                        Scope* parent) {
    auto [scope, genParams] = makeGenScope(facet.genParams(), parent);
    auto* impl =
        ctx.make<GenTraitImpl>(&facet, parent, scope, std::move(genParams));
    auto* implFacet = cast<TraitImplTypeFacet const*>(facet.definition());
    PRISM_ASSERT(implFacet->body());
    declareChildren(impl->associatedScope(), implFacet->body()->elems());
    return impl;
}

Symbol* GlobalDeclDeclare::doDeclare(GenParamDeclFacet const& facet,
                                     Scope* parent) {
    auto* trait = analyzeFacetAs<Trait>(*this, parent, facet.requirements());
    auto name = sourceContext->getTokenStr(facet.name());
    return ctx.make<GenericTypeParam>(std::string(name), &facet, parent, trait);
}

GlobalDeclDeclare::GenCtxAnaResult GlobalDeclDeclare::makeGenScope(
    GenParamListFacet const* paramDecls, Scope* parent) {
    PRISM_EXPECT(paramDecls);
    auto* scope = ctx.makeScope(parent);
    auto params = paramDecls->elems() | transform(FN1(&, declare(_1, scope)));
    return { scope, params | ToSmallVector<> };
}

static void declareGlobals(SemaContext& ctx, DiagnosticHandler& diagHandler,
                           Scope* globalScope,
                           std::span<SourceFilePair const> input) {
    GlobalDeclDeclare{ { ctx, diagHandler }, globalScope }.run(input);
}

// MARK: - Name resolution for global names

namespace prism {

struct GlobalNameResolver: InstantiationBase {
    DependencyGraph& dependencies;

    DependencyNode* getNode(Symbol& sym) { return dependencies.getNode(sym); }
    void addDependency(DependencyNode* node, Symbol* dependsOn);
    void addDependency(Symbol& symbol, Symbol* dependsOn);
    void resolve(Symbol* symbol);
    void doResolve(Symbol&) {}
    void declareGlobalVar(Scope* parent, VarDeclFacet const& facet);
    void doResolve(SourceFile& sourceFile);
    void doResolve(TraitImpl& impl);
    void doResolve(GenTraitImpl& impl);
    void resolveInterface(TraitImplInterface& interface);
    Symbol* declareBase(Scope* scope, BaseDeclFacet const& decl);
    MemberVar* declareMemberVar(Scope* scope, VarDeclFacet const& decl);
    void declareMembers(Symbol& typeOrTrait,
                        utl::function_view<void(Symbol*)> verify);
    void doResolve(CompositeType& type);
    void doResolve(GenCompositeType& type);
    void resolveInterface(CompTypeInterface& interface);
    void doResolve(Trait& trait);
    void doResolve(GenTrait& trait);
    void resolveInterface(TraitInterface& interface);
    FuncParam* analyzeParam(Symbol* parentSymbol, ParamDeclFacet const* facet,
                            Scope* scope, size_t index);
    FuncParam* doAnalyzeParam(Symbol* parentSymbol,
                              NamedParamDeclFacet const& param, Scope* scope,
                              size_t index);
    FuncParam* doAnalyzeParam(Symbol* parentSymbol,
                              ThisParamDeclFacet const& param, Scope* scope,
                              size_t index);
    void doResolve(Function& func);
    void doResolve(GenFuncImpl& genfunc);
    void resolveInterface(Symbol* parentSymbol, FuncInterface& interface,
                          FuncDeclBaseFacet const& funcFacet, Scope* scope);
    void resolveChildren(std::span<Symbol* const> symbols);
    void resolveChildren(auto& symbol);
};

} // namespace prism

void GlobalNameResolver::addDependency(DependencyNode* node,
                                       Symbol* dependsOn) {
    if (dependsOn && !isBuiltinSymbol(*dependsOn)) {
        node->addDependency(getNode(*dependsOn));
    }
}

void GlobalNameResolver::addDependency(Symbol& symbol, Symbol* dependsOn) {
    addDependency(getNode(symbol), dependsOn);
}

void GlobalNameResolver::resolve(Symbol* symbol) {
    if (!symbol) return;
    visit(*symbol, [this](auto& symbol) { doResolve(symbol); });
}

void GlobalNameResolver::declareGlobalVar(Scope* parent,
                                          VarDeclFacet const& facet) {
    if (!facet.typespec()) PRISM_UNIMPLEMENTED();
    auto* type = analyzeFacetAs<ValueType>(*this, parent, facet.typespec());
    auto* var = ctx.make<Variable>(getName(facet), &facet, parent,
                                   QualType{ type, {} });
    addDependency(*var, type);
}

void GlobalNameResolver::doResolve(SourceFile& sourceFile) {
    sourceContext = &sourceFile.sourceContext();
    auto globals = sourceFile.facet()->decls() | csp::filter<VarDeclFacet>;
    for (auto* decl: globals)
        declareGlobalVar(sourceFile.associatedScope(), *decl);
    resolveChildren(sourceFile);
}

void GlobalNameResolver::doResolve(TraitImpl& impl) {
    resolveInterface(impl.interface());
}

void GlobalNameResolver::doResolve(GenTraitImpl& impl) {
    resolveInterface(impl.interface());
}

void GlobalNameResolver::resolveInterface(TraitImplInterface& interface) {
    auto& impl = interface.traitImpl();
    auto* facet = cast<TraitImplFacet const*>(impl.facet());
    auto* def = cast<TraitImplTypeFacet const*>(facet->definition());
    interface._trait = analyzeFacetAs<Trait>(*this, impl.associatedScope(),
                                             def->traitDeclRef());
    interface._conf = analyzeFacetAs<CompositeType>(*this,
                                                    impl.associatedScope(),
                                                    def->conformingTypename());
    auto* node = getNode(impl);
    addDependency(node, interface._trait);
    addDependency(node, interface._conf);
    resolveChildren(impl);
}

Symbol* GlobalNameResolver::declareBase(Scope* scope,
                                        BaseDeclFacet const& decl) {
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
    // FIXME: We should expect types or traits but we have no interface for that
    diagHandler.push<BadSymRef>(sourceContext, decl.type(), base,
                                SymbolType::Trait);
    return nullptr;
}

MemberVar* GlobalNameResolver::declareMemberVar(Scope* scope,
                                                VarDeclFacet const& decl) {
    auto* type = analyzeFacetAs<ValueType>(*this, scope, decl.typespec());
    auto* var = ctx.make<MemberVar>(getName(decl), &decl, scope, type);
    addDependency(*var, type);
    return var;
}

void GlobalNameResolver::declareMembers(
    Symbol& typeOrTrait, utl::function_view<void(Symbol*)> verify) {
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

void GlobalNameResolver::doResolve(CompositeType& type) {
    resolveInterface(type.interface());
}

void GlobalNameResolver::doResolve(GenCompositeType& type) {
    resolveInterface(type.interface());
}

void GlobalNameResolver::resolveInterface(CompTypeInterface& interface) {
    auto& type = interface.compositeType();
    declareMembers(type, [&](Symbol* sym) {
        if (auto* basetrait = dyncast<BaseTrait*>(sym))
            interface._baseTraits.push_back(basetrait);
        else if (auto* baseclass = dyncast<BaseClass*>(sym))
            interface._bases.push_back(baseclass);
        else if (auto* memvar = dyncast<MemberVar*>(sym))
            interface._memvars.push_back(memvar);
    });
    resolveChildren(type);
}

void GlobalNameResolver::doResolve(Trait& trait) {
    resolveInterface(trait.interface());
}

void GlobalNameResolver::doResolve(GenTrait& trait) {
    resolveInterface(trait.interface());
}

void GlobalNameResolver::resolveInterface(TraitInterface& interface) {
    auto& trait = interface.trait();
    declareMembers(trait, [&](Symbol* sym) {
        if (auto* baseclass = dyncast<BaseClass*>(sym))
            PRISM_UNIMPLEMENTED(); // Error
        else if (auto* memvar = dyncast<MemberVar*>(sym))
            PRISM_UNIMPLEMENTED(); // Error
    });
    resolveChildren(trait);
}

FuncParam* GlobalNameResolver::analyzeParam(Symbol* parentSymbol,
                                            ParamDeclFacet const* facet,
                                            Scope* scope, size_t index) {
    if (!facet) return nullptr;
    return visit(*facet,
                 FN1(&, doAnalyzeParam(parentSymbol, _1, scope, index)));
}

FuncParam* GlobalNameResolver::doAnalyzeParam(Symbol* /* parentSymbol */,
                                              NamedParamDeclFacet const& param,
                                              Scope* scope,
                                              size_t /* index */) {
    auto* type = analyzeFacetAs<Type>(*this, scope, param.typespec());
    auto name = sourceContext->getTokenStr(param.name());
    return ctx.make<FuncParam>(std::string(name), &param, type,
                               FuncParam::Options{ .hasMut = false,
                                                   .isThis = false });
}

FuncParam* GlobalNameResolver::doAnalyzeParam(Symbol* parentSymbol,
                                              ThisParamDeclFacet const& param,
                                              Scope*, size_t index) {
    if (index != 0) {
        diagHandler.push<ThisParamBadPosition>(sourceContext, &param);
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
    if (!parentSymbol) PRISM_UNIMPLEMENTED();
    // clang-format off
    auto* thisType = visit<ValueType const*>(*parentSymbol, csp::overload{
        [&](UserType& type) { return &type; },
        [&](Trait& trait) { return ctx.getDynTraitType(&trait); },
        [&](TraitImpl& impl) { return impl.conformingType(); },
        [](Symbol const&) { PRISM_UNIMPLEMENTED(); }
    }); // clang-format on
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

void GlobalNameResolver::doResolve(Function& func) {
    resolveInterface(func.parentScope()->assocSymbol(), func.interface(),
                     *func.facet(), func.parentScope());
}

void GlobalNameResolver::doResolve(GenFuncImpl& genfunc) {
    resolveInterface(genfunc.parentScope()->assocSymbol(), genfunc.interface(),
                     *genfunc.facet(), genfunc.associatedScope());
}

void GlobalNameResolver::resolveInterface(Symbol* parentSymbol,
                                          FuncInterface& interface,
                                          FuncDeclBaseFacet const& funcFacet,
                                          Scope* scope) {
    if (auto* paramDecls = funcFacet.params())
        interface._params =
            paramDecls->elems() | enumerate |
            transform(FN1(&, analyzeParam(parentSymbol, _1.second, scope,
                                          _1.first))) |
            ToSmallVector<>;
    auto* retType = [&]() -> Type const* {
        if (auto* retFacet = funcFacet.retType())
            return analyzeFacetAs<Type>(*this, scope, retFacet);
        return ctx.getVoid();
    }();
    interface._sig = FuncSig::Compute(retType, interface.params());
}

void GlobalNameResolver::resolveChildren(std::span<Symbol* const> symbols) {
    for (auto* symbol: symbols)
        resolve(symbol);
}

void GlobalNameResolver::resolveChildren(auto& symbol) {
    // Make a copy of the symbols here because during resolution new symbols may
    // be added, invalidating the backing storage
    resolveChildren(symbol.associatedScope()->symbols() | ToSmallVector<>);
}

static DependencyGraph resolveGlobalNames(MonotonicBufferResource& resource,
                                          SemaContext& ctx,
                                          DiagnosticHandler& diagHandler,
                                          Scope* globalScope) {
    DependencyGraph dependencies(resource);
    GlobalNameResolver{ { ctx, diagHandler }, dependencies }.resolveChildren(
        globalScope->symbols());
    return dependencies;
}

// MARK: - Instantiate types

namespace prism {

struct InstantiationContext: AnalysisBase {
    void instantiate(Symbol&) {}

    using LayoutAccumulator =
        utl::function_view<TypeLayout(TypeLayout, TypeLayout)>;

    TypeLayout computeLayout(CompositeType const& type, LayoutAccumulator acc);
    void instantiate(StructType& type);
};

} // namespace prism

static size_t align(size_t size, size_t al) {
    if (al == 0) {
        PRISM_ASSERT(size == 0, "Align == 0 must imply size == 0");
        return size;
    }
    if (size % al == 0) return size;
    return size + al - size % al;
}

TypeLayout InstantiationContext::computeLayout(CompositeType const& type,
                                               LayoutAccumulator acc) {
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

void InstantiationContext::instantiate(StructType& type) {
    auto layout = computeLayout(type, [](TypeLayout curr, TypeLayout next) {
        size_t size = align(curr.size(), next.alignment()) + next.size();
        size_t align = std::max(curr.alignment(), next.alignment());
        return TypeLayout{ size, size, align };
    });
    type.setLayout(layout);
}

static void instantiateSymbol(SemaContext& ctx, DiagnosticHandler& diagHandler,
                              Symbol* symbol) {
    InstantiationContext instctx{ ctx, diagHandler };
    visit(*symbol, [&](auto& symbol) { instctx.instantiate(symbol); });
}

ConstructionResult prism::constructTarget(
    MonotonicBufferResource& resource, SemaContext& ctx,
    DiagnosticHandler& diagHandler, std::span<SourceFilePair const> input) {
    auto* target = ctx.make<Target>(ctx, "TARGET");
    declareBuiltins(ctx, target->associatedScope());
    makeCoreLibrary(ctx, target->associatedScope());
    declareGlobals(ctx, diagHandler, target->associatedScope(), input);
    auto dependencies = resolveGlobalNames(resource, ctx, diagHandler,
                                           target->associatedScope());
    dependencies.topsort();
    if (dependencies.hasCycle()) {
        diagHandler.push<TypeDefCycle>(dependencies.getCycle());
        return ConstructionResult::Fatal(target);
    }
    for (auto* symbol: dependencies.getTopoOrder() | ranges::views::reverse)
        instantiateSymbol(ctx, diagHandler, symbol);
    return { target, std::move(dependencies) };
}
