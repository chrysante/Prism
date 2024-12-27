#include "Prism/Sema/Construction.h"

#include <span>
#include <vector>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>
#include <utl/scope_guard.hpp>
#include <utl/stack.hpp>

#include "Prism/Common/Allocator.h"
#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/ExprAnalysis.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaIssue.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;
using ranges::views::zip;

static void declareBuiltins(SemaContext& ctx) {
    using enum Signedness;
#define SEMA_BUILTIN_TYPE(Name, Spelling, SymType, ...)                        \
    ctx.makeBuiltin<SymType>(BuiltinSymbol::Name, Spelling,                    \
                             nullptr __VA_OPT__(, ) __VA_ARGS__);
#include "Prism/Sema/Builtins.def"
}

static std::string getName(SourceContext const& sourceContext,
                           std::derived_from<DeclFacet> auto const& facet) {}

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
        ranges::for_each(children,
                         [&](Facet const* facet) { declare(scope, facet); });
    }

    void declareFile(SourceFileFacet const& facet,
                     SourceContext const& sourceContext) {
        this->sourceContext = &sourceContext;
        auto* file = ctx.make<SourceFile>(sourceContext.filepath().string(),
                                          &facet, globalScope, sourceContext);
        declareChildren(file->associatedScope(), facet.decls());
    }

    void declare(Scope* scope, Facet const* facet) {
        if (!facet) return;
        visit(*facet, [&](auto const& facet) { declareImpl(scope, facet); });
    }

    void declareImpl(Scope*, Facet const&) {}

    void declareImpl(Scope* parent, FuncDefFacet const& facet) {
        if (facet.body() && isa<CompoundFacet>(facet.body()))
            ctx.make<FunctionImpl>(getName(facet), &facet, parent,
                                   utl::small_vector<FuncParam*>{}, nullptr);
        else
            ctx.make<Function>(getName(facet), &facet, parent,
                               utl::small_vector<FuncParam*>{}, nullptr);
    }

    void declareImpl(Scope* parent, CompTypeDeclFacet const& facet) {
        auto* type = [&]() -> Symbol* {
            switch (facet.declarator().kind) {
            case TokenKind::Struct:
                return ctx.make<StructType>(getName(facet), &facet, parent);
            case TokenKind::Trait:
                return ctx.make<Trait>(getName(facet), &facet, parent);
            default:
                PRISM_UNREACHABLE();
            }
        }();
        declareChildren(type->associatedScope(), facet.body()->elems());
    }

    void declareImpl(Scope* parent, TraitImplFacet const& facet) {
        auto* impl = ctx.make<TraitImpl>(&facet, parent, nullptr, nullptr);
        declareChildren(impl->associatedScope(),
                        cast<TraitImplTypeFacet const*>(facet.definition())
                            ->body()
                            ->elems());
    }
};

} // namespace

static void declareGlobals(SemaContext& ctx, IssueHandler& iss,
                           Scope* globalScope,
                           std::span<SourceFilePair const> input) {
    GloablDeclDeclare{ { ctx, iss }, globalScope }.run(input);
}

namespace {

class DependencyNode {
public:
    explicit DependencyNode(Symbol* symbol, MonotonicBufferResource& resource):
        sym(symbol), succs(&resource) {}

    void addDependency(DependencyNode const* node) {
        if (node) succs.push_back(node);
    }

    Symbol* symbol() const { return sym; }

    std::span<DependencyNode const* const> successors() const { return succs; }

private:
    using AllocType =
        ResourceAllocator<DependencyNode const*, MonotonicBufferResource>;
    std::vector<DependencyNode const*, AllocType> succs;
    Symbol* sym;
};

class DependencyGraph {
public:
    DependencyGraph(MonotonicBufferResource& resource): nodes(&resource) {}

    DependencyNode* getNode(Symbol* sym) {
        auto itr = nodes.find(sym);
        if (itr != nodes.end()) return itr->second;
        auto* resource = getResource();
        auto* node = allocate<DependencyNode>(*resource, sym, *resource);
        nodes.insert({ sym, node });
        return node;
    }

private:
    using AllocType =
        ResourceAllocator<std::pair<Symbol const*, DependencyNode*>,
                          MonotonicBufferResource>;

    MonotonicBufferResource* getResource() const {
        return nodes.get_allocator().resource();
    }

    utl::hashmap<Symbol const*, DependencyNode*, utl::hash<Symbol const*>,
                 std::equal_to<>, AllocType>
        nodes;
};

} // namespace

namespace prism {

struct GlobalNameResolver: InstantiationBase {
    DependencyGraph& dependencies;

    DependencyNode* getNode(Symbol* sym) { return dependencies.getNode(sym); }

    void resolve(Symbol* symbol) {
        if (!symbol) return;
        visit(*symbol, [this](auto& symbol) { resolveImpl(symbol); });
    }

    void resolveImpl(Symbol&) {}

    void declareGlobalVar(Scope* parent, VarDeclFacet const& facet) {
        if (!facet.typespec()) {
            PRISM_UNIMPLEMENTED();
        }
        auto* type = analyzeFacetAs<ValueType>(*this, parent, facet.typespec());
        auto* var = ctx.make<Variable>(getName(facet), &facet, parent,
                                       QualType{ type, {} });
        getNode(var)->addDependency(getNode(type));
    }

    void resolveImpl(SourceFile& sourceFile) {
        sourceContext = &sourceFile.sourceContext();
        auto* node = getNode(&sourceFile);
        auto globals = sourceFile.facet()->decls() | csp::filter<VarDeclFacet>;
        for (auto* decl: globals)
            declareGlobalVar(sourceFile.associatedScope(), *decl);
        resolveChildren(sourceFile);
    }

    void resolveImpl(TraitImpl& impl) {
        auto* def = cast<TraitImplTypeFacet const*>(impl.facet()->definition());
        impl._trait = analyzeFacetAs<Trait>(*this, impl.parentScope(),
                                            def->traitDeclRef());
        impl._conf = analyzeFacetAs<UserType>(*this, impl.parentScope(),
                                              def->conformingTypename());
        auto* node = getNode(&impl);
        node->addDependency(getNode(impl._trait));
        node->addDependency(getNode(impl._conf));
        resolveChildren(impl);
    }

    BaseClass* declareBaseClass(Scope* scope, BaseDeclFacet const& decl) {
        auto* basetype = analyzeFacetAs<UserType>(*this, scope, decl.type());
        auto* base = ctx.make<BaseClass>(&decl, scope, basetype);
        getNode(base)->addDependency(getNode(basetype));
        return base;
    }

    MemberVar* declareMemberVar(Scope* scope, VarDeclFacet const& decl) {
        auto* type = analyzeFacetAs<UserType>(*this, scope, decl.typespec());
        auto* var = ctx.make<MemberVar>(getName(decl), &decl, scope, type);
        getNode(var)->addDependency(getNode(type));
        return var;
    }

    void resolveImpl(UserType& type) {
        auto* scope = type.associatedScope();
        auto* node = getNode(&type);
        if (auto* bases = type.facet()->bases()) {
            for (auto* decl: bases->elems()) {
                auto* base = declareBaseClass(scope, *decl);
                node->addDependency(getNode(base));
            }
        }
        if (auto* body = type.facet()->body()) {
            for (auto* decl: body->elems() | csp::filter<VarDeclFacet>) {
                auto* var = declareMemberVar(scope, *decl);
                node->addDependency(getNode(var));
            }
        }
        resolveChildren(type);
    }

    void resolveImpl(Trait& trait) { resolveChildren(trait); }

    void resolveImpl(Variable& var) {
        if (!var.facet()->typespec()) {
            PRISM_UNIMPLEMENTED();
            return;
        }
        auto* type = analyzeFacetAs<Type>(*this, var.parentScope(),
                                          var.facet()->typespec());
        // For now we assume no references
        auto* valtype = cast<ValueType*>(type);
        var._type = { valtype, Mutability::Mut };
    }

    FuncParam* analyzeParam(Function& func, ParamDeclFacet const* facet) {
        if (!facet) return nullptr;
        return visit(*facet, [&](auto& facet) {
            return analyzeParamImpl(func, facet);
        });
    }

    FuncParam* analyzeParamImpl(Function& func,
                                NamedParamDeclFacet const& param) {
        auto* type =
            analyzeFacetAs<Type>(*this, func.parentScope(), param.typespec());
        auto name = sourceContext->getTokenStr(param.name());
        return ctx.make<FuncParam>(std::string(name), &param, type,
                                   /* mut: */ false);
    }

    FuncParam* analyzeParamImpl(Function& func,
                                ThisParamDeclFacet const& param) {
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
            return ctx.make<FuncParam>("this", &param, type, /* mut: */ false);
        }
        else {
            return ctx.make<FuncParam>("this", &param, thisType,
                                       mut == Mutability::Mut);
        }
    }

    void resolveImpl(Function& func) {
        if (auto* paramDecls = func.facet()->params())
            func._params = paramDecls->elems() |
                           transform(REFFN(analyzeParam, func)) |
                           ToSmallVector<>;
        if (auto* retFacet = func.facet()->retType())
            func._retType =
                analyzeFacetAs<Type>(*this, func.parentScope(), retFacet);
        else
            func._retType = ctx.getVoid();
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

static void resolveGlobalNames(SemaContext& ctx, IssueHandler& iss,
                               Scope* globalScope) {
    MonotonicBufferResource resource;
    DependencyGraph dependencies(resource);
    GlobalNameResolver{ { ctx, iss }, dependencies }.resolveChildren(
        globalScope->symbols());
}

static DependencyNode* buildDependencyGraph(MonotonicBufferResource& alloc,
                                            Target* target) {
    return nullptr;
}

Target* prism::constructTarget(SemaContext& ctx, IssueHandler& iss,
                               std::span<SourceFilePair const> input) {
    auto* target = ctx.make<Target>(ctx, "TARGET");
    declareBuiltins(ctx);
    declareGlobals(ctx, iss, target->associatedScope(), input);
    resolveGlobalNames(ctx, iss, target->associatedScope());
    return target;
}
