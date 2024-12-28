#include "Prism/Sema/ExprAnalysis.h"

#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/NameLookup.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaIssue.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

static Facet const* getDeclName(Facet const* facet) {
    if (!facet) return nullptr;
    return visit(*facet, [](auto const& facet) -> Facet const* {
        if constexpr (requires { facet.nameFacet(); }) {
            return facet.nameFacet();
        }
        else if constexpr (requires { facet.name(); }) {
            return facet.name();
        }
        else {
            return &facet;
        }
    });
}

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
            auto* issue = iss.push<UndeclaredID>(*sourceContext, &id);
            if (auto* similar = symbols.similar()) {
                auto* name = getDeclName(similar->facet());
                issue
                    ->addNote([=](std::ostream& str) {
                    str << "Did you mean \'" << formatName(*similar) << "\'?";
                })->addNote(name, [=](std::ostream& str) {
                    str << formatName(*similar) << " declared here";
                });
            }
            return nullptr;
        }
        if (symbols.isSingleSymbol()) return symbols.singleSymbol();
        PRISM_UNIMPLEMENTED();
    }

    Symbol* analyzeImpl(TerminalFacet const& term) {
        switch (term.token().kind) {
#define SEMA_BUILTIN_TYPE(Name, Spelling, SymType, ...)                        \
    case TokenKind::Name:                                                      \
        return ctx.get##Name();
#include "Prism/Sema/Builtins.def"
        case TokenKind::Identifier:
            return analyzeID(term);
        default:
            PRISM_UNIMPLEMENTED();
        }
    }
};

} // namespace

Symbol* prism::analyzeFacet(AnalysisBase const& context, Scope* scope,
                            Facet const* facet) {
    return AnaContext{ context, scope }.analyze(facet);
}
