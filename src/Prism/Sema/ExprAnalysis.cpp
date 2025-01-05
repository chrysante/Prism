#include "Prism/Sema/ExprAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

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

    Symbol* doAnalyze(Facet const&) { PRISM_UNIMPLEMENTED(); }
    Symbol* analyzeID(TerminalFacet const& id);
    IntLiteral* analyzeIntLiteral(TerminalFacet const& term, int base);
    Symbol* doAnalyze(TerminalFacet const& term);
    Symbol* doAnalyze(CallFacet const& call);
};

} // namespace

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
        PRISM_UNIMPLEMENTED();
    }
}

Symbol* AnaContext::analyzeID(TerminalFacet const& id) {
    auto name = sourceContext->getTokenStr(id.token());
    auto symbols = unqualifiedLookup(scope, name);
    if (!symbols.success()) {
        if (symbols.isAmbiguous()) PRISM_UNIMPLEMENTED();
        DE.emit<UndeclaredID>(sourceContext, &id, symbols.similar());
        return nullptr;
    }
    if (symbols.isSingleSymbol()) return symbols.singleSymbol();
    if (symbols.isOverloadSet()) PRISM_UNIMPLEMENTED();
    PRISM_UNIMPLEMENTED();
}

IntLiteral* AnaContext::analyzeIntLiteral(TerminalFacet const& term, int base) {
    auto str = sourceContext->getTokenStr(term.token());
    auto value = APInt::parse(str, base, 32);
    if (!value) PRISM_UNIMPLEMENTED();
    return ctx.make<IntLiteral>(&term, *std::move(value), ctx.getInt32());
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
