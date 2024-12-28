#include "Prism/Sema/SemaIssue.h"

#include <ostream>
#include <string_view>

#include "Prism/Facet/Facet.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

static std::optional<SourceRange> getSourceRange(Facet const* facet) {
    if (!facet) return std::nullopt;
    auto find = [](Facet const* facet, auto inc) {
        while (!isa<TerminalFacet>(facet)) {
            PRISM_ASSERT(facet);
            facet = inc(facet);
        }
        return cast<TerminalFacet const*>(facet);
    };
    auto* first = find(facet, [](Facet const* facet) {
        PRISM_ASSERT(!facet->children().empty());
        return facet->childAt(0);
    });
    auto* last = find(facet, [](Facet const* facet) {
        PRISM_ASSERT(!facet->children().empty());
        return facet->children().back();
    });
    uint32_t beginIndex = first->token().index;
    uint32_t endIndex = last->token().index + last->token().sourceLen;
    return SourceRange{ beginIndex, endIndex - beginIndex };
}

SemaIssue::SemaIssue(Issue::Kind kind, SourceContext const& ctx,
                     Facet const* facet):
    Issue(kind, getSourceRange(facet)), ctx(ctx), fct(facet) {}

SemaNote* SemaIssue::addNote(Facet const* facet, utl::vstreammanip<> impl) {
    return addChild<SemaNote>(sourceContext(), facet, std::move(impl));
}

SemaNote::SemaNote(SourceContext const& ctx, Facet const* facet,
                   utl::vstreammanip<> impl):
    SemaIssue(Issue::Note, ctx, facet), impl(std::move(impl)) {}

void SemaNote::header(std::ostream& str, SourceContext const&) const {
    str << impl;
}

static std::string_view getID(Facet const* facet, SourceContext const& ctx) {
    auto* term = dyncast<TerminalFacet const*>(facet);
    if (!term || term->token().kind != TokenKind::Identifier) return {};
    return ctx.getTokenStr(term->token());
}

#include "Prism/Sema/SemaIssuesDef.inl"
