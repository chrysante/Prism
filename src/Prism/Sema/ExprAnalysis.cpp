#include "Prism/Sema/ExprAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/GenericInstantiation.h"
#include "Prism/Sema/NameLookup.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;
using ranges::views::zip;

namespace {

struct AnaContext: AnalysisBase {
    Scope* scope;
    ExprAnalysisOptions options;

    Symbol* analyze(Facet const* facet);

    template <std::derived_from<Symbol> S>
    S* analyzeAs(Facet const* facet) {
        auto* symbol = analyze(facet);
        return verifySymbolType<S>(symbol, facet);
    }

    template <std::derived_from<Symbol> S = Symbol, typename F = Facet>
    utl::small_vector<S*> analyzeList(std::span<F const* const> facets) {
        utl::small_vector<S*> result;
        for (auto* facet: facets)
            result.push_back(analyzeAs<S>(facet));
        return result;
    }

    template <std::derived_from<Symbol> S>
    S* verifySymbolType(Symbol* symbol, Facet const* facet) {
        return detail::verifySymbolType<S>(*this, facet, symbol);
    }

    Symbol* doAnalyze(Facet const&) { PRISM_UNREACHABLE(); }
    Symbol* analyzeID(TerminalFacet const& id);
    IntLiteral* analyzeIntLiteral(TerminalFacet const& term, int base);
    Symbol* doAnalyze(TerminalFacet const& term);
    Symbol* doAnalyze(BinaryFacet const& binary);
    Symbol* doAnalyzeScopeRes(BinaryFacet const& binary);
    Symbol* doAnalyze(FnTypeFacet const& facet);
    Symbol* doAnalyze(NamedParamDeclFacet const& declFacet);
    Symbol* doAnalyze(PrefixFacet const& prefix);
    Symbol* doAnalyze(CallFacet const& call);

    decltype(auto) withScope(Scope* tempScope, std::invocable auto&& f) {
        PRISM_ASSERT(tempScope, "must not be null");
        auto stashed = std::exchange(scope, tempScope);
        utl::scope_guard pop = [&] { scope = stashed; };
        return std::invoke(f);
    }
};

} // namespace

template <ranges::range R>
    requires std::is_pointer_v<ranges::range_value_t<R>>
static bool isNull(R&& ptrRange) {
    return ranges::any_of(ptrRange, FN1(, _1 == nullptr));
}

void detail::pushBadSymRef(AnalysisBase const& context, Facet const* facet,
                           Symbol* symbol, SymbolType expected) {
    context.DE.emit<BadSymRef>(context.sourceContext, facet, symbol, expected);
}

Symbol* prism::analyzeFacet(AnalysisBase const& context, Scope* scope,
                            Facet const* facet, ExprAnalysisOptions options) {
    return AnaContext{ context, scope, options }.analyze(facet);
}

Symbol* AnaContext::analyze(Facet const* facet) {
    if (!facet) return nullptr;
    return visit(*facet, FN1(&, doAnalyze(_1)));
}

Symbol* AnaContext::doAnalyze(TerminalFacet const& term) {
    switch (term.token().kind) {
#define SEMA_BUILTIN(Name, Spelling, SymType, ...)                             \
    case TokenKind::Name:                                                      \
        return ctx.get##Name();
#include "Prism/Sema/Builtins.def"
    case TokenKind::Identifier:
        return analyzeID(term);
    case TokenKind::IntLiteralBin:
        return analyzeIntLiteral(term, 2);
    case TokenKind::IntLiteralDec:
        return analyzeIntLiteral(term, 10);
    case TokenKind::IntLiteralHex:
        return analyzeIntLiteral(term, 16);
    default:
        PRISM_UNREACHABLE();
    }
}

Symbol* AnaContext::doAnalyze(BinaryFacet const& binary) {
    if (binary.operationFacet() && binary.operation().kind == TokenKind::Period)
        return doAnalyzeScopeRes(binary);
    auto* LHS = analyze(binary.LHS());
    auto* RHS = analyze(binary.RHS());
    if (!binary.operationFacet() || !LHS || !RHS) return nullptr;
    Token op = binary.operation();
    switch (op.kind) {
    default:
        PRISM_UNREACHABLE();
    }
}

Symbol* AnaContext::doAnalyzeScopeRes(BinaryFacet const& binary) {
    PRISM_EXPECT(binary.operationFacet() &&
                 binary.operation().kind == TokenKind::Period);
    auto* LHS = analyze(binary.LHS());
    if (!LHS) return nullptr;
    if (!LHS->associatedScope()) PRISM_UNIMPLEMENTED(); // TODO: push error
    return withScope(LHS->associatedScope(),
                     [&] { return analyze(binary.RHS()); });
}

Symbol* AnaContext::doAnalyze(FnTypeFacet const& facet) {
    auto* retType = analyzeAs<Type>(facet.retType());
    if (!facet.paramList()) return nullptr;
    auto argTypes = analyzeList<Type const>(facet.paramList()->elems());
    if (!retType || isNull(argTypes)) return nullptr;
    return ctx.make<FunctionType>(&facet, nullptr, retType,
                                  std::move(argTypes));
}

Symbol* AnaContext::doAnalyze(NamedParamDeclFacet const& declFacet) {
    if (isa<FnTypeFacet>(declFacet.parent()->parent()))
        return analyzeAs<Type>(declFacet.typespec());
    PRISM_UNIMPLEMENTED();
}

template <typename T, typename... Args>
concept AnyOf = (std::same_as<T, Args> || ...);

Symbol* AnaContext::analyzeID(TerminalFacet const& id) {
    auto name = sourceContext->getTokenStr(id.token());
    auto symbols = unqualifiedLookup(scope, name);
    // clang-format off
    using NLR = NameLookupResult;
    return symbols.visit(csp::overload{
        [&](AnyOf<NLR::None, NLR::Similar> auto) -> Symbol* {
            DE.emit<UndeclaredID>(sourceContext, &id, symbols.similar());
            return nullptr;
        },
        [&](Symbol* symbol) -> Symbol* { return symbol; },
        [&](std::span<Function* const> /* overloadSet */) -> Symbol* {
            PRISM_UNIMPLEMENTED();
        },
        [&](std::span<Symbol const* const> ambiSet) -> Symbol* {
            DE.emit<AmbiguousNameLookup>(sourceContext, &id, ambiSet);
            return nullptr;
        },
    }); // clang-format on
}

IntLiteral* AnaContext::analyzeIntLiteral(TerminalFacet const& term, int base) {
    auto str = sourceContext->getTokenStr(term.token());
    auto value = APInt::parse(str, base, 32);
    if (!value) PRISM_UNIMPLEMENTED();
    return ctx.make<IntLiteral>(&term, *std::move(value), ctx.getInt32());
}

Symbol* AnaContext::doAnalyze(PrefixFacet const& prefix) {
    auto* operand = analyze(prefix.operand());
    if (!operand) return nullptr;
    PRISM_UNIMPLEMENTED();
}

Symbol* AnaContext::doAnalyze(CallFacet const& call) {
    auto* callee = analyze(call.callee());
    auto args = call.arguments()->elems() | transform(FN1(&, analyze(_1))) |
                ToSmallVector<>;
    if (!callee || !ranges::all_of(args, ToAddress)) return nullptr;
    if (auto* gensym = dyncast<GenericSymbol*>(callee)) {
        if (options.instantiateGenericsLazily)
            return instantiateGenericLazy(ctx, *gensym, args);
        return instantiateGeneric(ctx, DE, *gensym, &call, args);
    }
    PRISM_UNIMPLEMENTED();
}
