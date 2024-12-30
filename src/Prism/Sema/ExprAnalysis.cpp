#include "Prism/Sema/ExprAnalysis.h"

#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/NameLookup.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaIssue.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

namespace {

struct AnaContext: AnalysisBase {
    Scope* scope;

    Symbol* analyze(Facet const* facet) {
        if (!facet) return nullptr;
        return visit(*facet,
                     [this](auto const& facet) { return analyzeImpl(facet); });
    }

    Symbol* analyzeImpl(Facet const&) { PRISM_UNIMPLEMENTED(); }

    Symbol* analyzeID(TerminalFacet const& id) {
        auto name = sourceContext->getTokenStr(id.token());
        auto symbols = unqualifiedLookup(scope, name);
        if (!symbols.success()) {
            iss.push<UndeclaredID>(sourceContext, &id, symbols.similar());
            return nullptr;
        }
        if (symbols.isSingleSymbol()) return symbols.singleSymbol();
        PRISM_UNIMPLEMENTED();
    }

    IntLiteral* analyzeIntLiteral(TerminalFacet const& term, int base) {
        auto str = sourceContext->getTokenStr(term.token());
        auto value = APInt::parse(str, base, 32);
        if (!value) PRISM_UNIMPLEMENTED();
        return ctx.make<IntLiteral>(&term, *std::move(value), ctx.getInt32());
    }

    Symbol* analyzeImpl(TerminalFacet const& term) {
        switch (term.token().kind) {
#define SEMA_BUILTIN_TYPE(Name, Spelling, SymType, ...)                        \
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
};

} // namespace

void detail::pushBadSymRef(AnalysisBase const& context, Facet const* facet,
                           Symbol* symbol, SymbolType expected) {
    context.iss.push<BadSymRef>(context.sourceContext, facet, symbol, expected);
}

Symbol* prism::analyzeFacet(AnalysisBase const& context, Scope* scope,
                            Facet const* facet) {
    return AnaContext{ context, scope }.analyze(facet);
}
