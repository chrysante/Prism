#include "Prism/Sema/SemaIssue.h"

#include <ostream>
#include <string_view>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Functional.h"
#include "Prism/Common/PrettyName.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Sema/Symbol.h"
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
    auto findNonNullChild = [](auto&& children) {
        auto itr = ranges::find_if(children, ToAddress);
        PRISM_ASSERT(itr != children.end());
        return *itr;
    };
    auto* first = find(facet, VALFN1(findNonNullChild(_1->children())));
    auto* last = find(facet, VALFN1(findNonNullChild(_1->children() |
                                                     ranges::views::reverse)));
    uint32_t beginIndex = first->token().index;
    uint32_t endIndex = last->token().index + last->token().sourceLen;
    return SourceRange{ beginIndex, endIndex - beginIndex };
}

SemaIssue::SemaIssue(Issue::Kind kind, SourceContext const* ctx,
                     Facet const* facet):
    Issue(kind, getSourceRange(facet)), ctx(ctx), fct(facet) {}

SemaNote* SemaIssue::addNote(Facet const* facet, utl::vstreammanip<> impl) {
    return addChild<SemaNote>(sourceContext(), facet, std::move(impl));
}

SemaHint* SemaIssue::addHint(Facet const* facet, utl::vstreammanip<> impl) {
    return addChild<SemaHint>(sourceContext(), facet, std::move(impl));
}

SemaMessage::SemaMessage(Issue::Kind kind, SourceContext const* ctx,
                         Facet const* facet, utl::vstreammanip<> impl):
    SemaIssue(kind, ctx, facet), impl(std::move(impl)) {}

void SemaMessage::header(std::ostream& str, SourceContext const*) const {
    str << impl;
}

static std::string_view getID(Facet const* facet, SourceContext const* ctx) {
    if (!ctx) return {};
    auto* term = dyncast<TerminalFacet const*>(facet);
    if (!term || term->token().kind != TokenKind::Identifier) return {};
    return ctx->getTokenStr(term->token());
}

#include "Prism/Sema/SemaIssuesDef.inl"
