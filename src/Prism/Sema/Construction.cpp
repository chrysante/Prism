#include "Prism/Sema/Construction.h"

#include <vector>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/scope_guard.hpp>
#include <utl/stack.hpp>

#include "Prism/Common/Allocator.h"
#include "Prism/Common/Assert.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

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
        auto* scope = ctx.make<Scope>();
        auto* file = ctx.make<SourceFile>(sourceContext.filepath().string(),
                                          &facet, sourceContext, target, scope);
        target->associatedScope()->addSymbol(file);
        declareChildren(scope, facet.decls());
    }

    void declare(Scope* scope, Facet const* facet) {
        if (!facet) return;
        visit(*facet, [&](auto const& facet) { declareImpl(scope, facet); });
    }

    void declareImpl(Scope*, Facet const&) {}

    std::string getName(DeclFacet const& facet) const {
        auto token = cast<TerminalFacet const&>(*facet.name()).token();
        PRISM_ASSERT(token.kind == TokenKind::Identifier);
        return std::string(sourceContext->getTokenStr(token));
    }

    void declareImpl(Scope* parent, VarDeclFacet const& facet) {
        auto* var = ctx.make<Variable>(getName(facet), &facet, parent, nullptr);
        parent->addSymbol(var);
    }
    void declareImpl(Scope* parent, FuncDeclFacet const& facet) {
        auto* func =
            ctx.make<Function>(getName(facet), &facet, parent, nullptr);
        parent->addSymbol(func);
    }

    void declareImpl(Scope* parent, CompTypeDeclFacet const& facet) {
        auto* scope = ctx.make<Scope>();
        auto* type = [&]() -> CompositeType* {
            switch (facet.declarator().kind) {
            case TokenKind::Struct:
                return ctx.make<StructType>(getName(facet), &facet, parent,
                                            scope);
            default:
                PRISM_ASSERT(false);
            }
        }();
        parent->addSymbol(type);
        declareChildren(scope, facet.body()->elems());
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
    auto* globalScope = ctx.make<Scope>();
    auto* target = ctx.make<Target>("TARGET", globalScope);
    declareBuiltins(ctx, target);
    declareGlobals(ctx, target, input);

    return target;
}
