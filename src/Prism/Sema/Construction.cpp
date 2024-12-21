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
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;

static void declareBuiltins(SemaContext& ctx, Target* target) {}

namespace {

struct GloablDeclDeclare {
    SemaContext& ctx;
    Target* target;
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
        auto* file =
            ctx.make<SourceFile>(ctx, sourceContext.filepath().string(), &facet,
                                 target->associatedScope(), sourceContext);
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
            ctx.make<FunctionImpl>(ctx, getName(facet), &facet, parent,
                                   params | ToSmallVector<>, nullptr);
        else
            ctx.make<Function>(getName(facet), &facet, parent,
                               params | ToSmallVector<>, nullptr);
    }

    void declareImpl(Scope* parent, CompTypeDeclFacet const& facet) {
        auto* type = [&]() -> Symbol* {
            switch (facet.declarator().kind) {
            case TokenKind::Struct:
                return ctx.make<StructType>(ctx, getName(facet), &facet,
                                            parent);
            case TokenKind::Trait:
                return ctx.make<Trait>(ctx, getName(facet), &facet, parent);
            default:
                PRISM_UNREACHABLE();
            }
        }();
        declareChildren(type->associatedScope(), facet.body()->elems());
    }

    void declareImpl(Scope* parent, TraitImplFacet const& facet) {
        auto* impl = ctx.make<TraitImpl>(ctx, &facet, parent, nullptr, nullptr);
        declareChildren(impl->associatedScope(),
                        cast<TraitTypeDeclFacet const*>(facet.declaration())
                            ->body()
                            ->elems());
    }
};

} // namespace

static void declareGlobals(SemaContext& ctx, Target* target,
                           std::span<SourceFilePair const> input) {
    GloablDeclDeclare{ ctx, target }.run(input);
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
    declareBuiltins(ctx, target);
    declareGlobals(ctx, target, input);
    return target;
}
