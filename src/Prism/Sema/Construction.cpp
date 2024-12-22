#include "Prism/Sema/Construction.h"

#include <vector>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/scope_guard.hpp>
#include <utl/stack.hpp>

#include "Prism/Common/Allocator.h"
#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/NameLookup.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;

static void declareBuiltins(SemaContext& ctx, Scope* scope) {
    ctx.make<ByteType>("byte", scope);
    using enum Signedness;
    ctx.make<IntType>("i8", scope, 8, Signed);
    ctx.make<IntType>("i16", scope, 16, Signed);
    ctx.make<IntType>("i32", scope, 32, Signed);
    ctx.make<IntType>("i64", scope, 64, Signed);
    ctx.make<IntType>("u8", scope, 8, Unsigned);
    ctx.make<IntType>("u16", scope, 16, Unsigned);
    ctx.make<IntType>("u32", scope, 32, Unsigned);
    ctx.make<IntType>("u64", scope, 64, Unsigned);
    ctx.make<FloatType>("f32", scope, 32);
    ctx.make<FloatType>("f64", scope, 64);
}

namespace {

struct GloablDeclDeclare {
    SemaContext& ctx;
    Scope* globalScope;
    SourceContext const* sourceContext = nullptr;

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

    std::string getName(std::derived_from<DeclFacet> auto const& facet) const {
        auto token = cast<TerminalFacet const&>(*facet.name()).token();
        PRISM_ASSERT(token.kind == TokenKind::Identifier);
        return std::string(sourceContext->getTokenStr(token));
    }

    void declareImpl(Scope* parent, VarDeclFacet const& facet) {
        ctx.make<Variable>(getName(facet), &facet, parent, nullptr);
    }

    FunctionParameter makeFuncParam(ParamDeclFacet const* decl) {
        if (!decl) return {};
        // clang-format off
        std::string name(visit(*decl, csp::overload{
            [&](NamedParamDeclFacet const& param) {
                return  sourceContext->getTokenStr(param.name());
            },
            [&](ThisParamDeclFacet const& param) { return "this"; }
        })); // clang-format on
        return { decl, std::move(name), nullptr };
    }

    void declareImpl(Scope* parent, FuncDefFacet const& facet) {
        auto params = facet.params()->elems() | transform(FN(makeFuncParam));
        if (facet.body() && isa<CompoundFacet>(facet.body()))
            ctx.make<FunctionImpl>(getName(facet), &facet, parent,
                                   params | ToSmallVector<>, nullptr);
        else
            ctx.make<Function>(getName(facet), &facet, parent,
                               params | ToSmallVector<>, nullptr);
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
                        cast<TraitTypeDeclFacet const*>(facet.declaration())
                            ->body()
                            ->elems());
    }
};

} // namespace

static void declareGlobals(SemaContext& ctx, Scope* globalScope,
                           std::span<SourceFilePair const> input) {
    GloablDeclDeclare{ ctx, globalScope }.run(input);
}

namespace prism {

struct GlobalNameResolver {
    SemaContext& ctx;
    SourceContext const* src;

    void resolve(Symbol* symbol) {
        if (!symbol) return;
        visit(*symbol, [this](auto& symbol) { resolveImpl(symbol); });
    }

    void resolveImpl(Symbol&) {}

    void resolveImpl(SourceFile& sourceFile) {
        src = &sourceFile.sourceContext();
        resolveChildren(sourceFile);
    }

    void resolveImpl(TraitImpl& impl) {
        auto* decl =
            cast<TraitTypeDeclFacet const*>(impl.facet()->declaration());
        auto* trait = lookup(decl->traitName(), impl.parentScope());
        auto* conf = lookup(decl->conformingTypename(), impl.parentScope());
        impl._trait = cast<Trait*>(trait);
        impl._conf = cast<UserType*>(conf);
    }

    void resolveImpl(Function& func) {}

    void resolveChildren(std::span<Symbol* const> symbols) {
        for (auto* symbol: symbols)
            resolve(symbol);
    }

    void resolveChildren(auto& symbol) {
        resolveChildren(symbol.associatedScope()->symbols());
    }

    Symbol* lookup(Facet const* nameFacet, Scope* scope) {
        if (auto* term = dyncast<TerminalFacet const*>(nameFacet)) {
            PRISM_ASSERT(term->token().kind == TokenKind::Identifier);
            auto name = src->getTokenStr(term->token());
            auto symbols = unqualifiedLookup(scope, name);
            if (!symbols.isSingleSymbol()) {
                PRISM_UNIMPLEMENTED();
            }
            return symbols.singleSymbol();
        }
        PRISM_UNIMPLEMENTED();
    }
};

} // namespace prism

static void resolveGlobalNames(SemaContext& ctx, Scope* globalScope) {
    GlobalNameResolver{ ctx }.resolveChildren(globalScope->symbols());
}

namespace {

struct DependencyNode {
    Symbol* symbol;
};

} // namespace

static DependencyNode* buildDependencyGraph(MonotonicBufferResource& alloc,
                                            Target* target) {
    return nullptr;
}

Target* prism::constructTarget(SemaContext& ctx,
                               std::span<SourceFilePair const> input) {
    auto* target = ctx.make<Target>(ctx, "TARGET");
    declareBuiltins(ctx, target->associatedScope());
    declareGlobals(ctx, target->associatedScope(), input);
    resolveGlobalNames(ctx, target->associatedScope());
    return target;
}
