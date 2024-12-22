#include "Prism/Sema/SemaIssue.h"

#include <ostream>
#include <string_view>

#include "Prism/Facet/Facet.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

static uint32_t getIndex(Facet const* facet) {
    while (!isa<TerminalFacet>(facet)) {
        facet = facet->childAt(0);
    }
    return cast<TerminalFacet const*>(facet)->token().index;
}

SemaIssue::SemaIssue(Issue::Kind kind, SourceContext const& ctx,
                     Facet const* facet):
    Issue(kind, getIndex(facet)), ctx(ctx), fct(facet) {}

static std::string_view getID(Facet const* facet, SourceContext const& ctx) {
    auto* term = dyncast<TerminalFacet const*>(facet);
    if (!term || term->token().kind != TokenKind::Identifier) return {};
    return ctx.getTokenStr(term->token());
}

#include "Prism/Sema/SemaIssuesDef.inl"
