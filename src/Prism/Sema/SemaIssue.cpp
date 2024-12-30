#include "Prism/Sema/SemaIssue.h"

#include <ostream>
#include <string_view>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Functional.h"
#include "Prism/Common/PrettyName.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;

static Facet const* findNonNullChild(auto&& children) {
    auto itr = ranges::find_if(children, ToAddress);
    PRISM_ASSERT(itr != children.end());
    return *itr;
};

static std::optional<SourceRange> getSourceRange(Facet const* facet) {
    if (!facet) return std::nullopt;
    auto find = [](Facet const* facet, auto inc) {
        while (!isa<TerminalFacet>(facet)) {
            PRISM_ASSERT(facet);
            facet = inc(facet);
        }
        return cast<TerminalFacet const*>(facet);
    };

    auto* first = find(facet, FN1(findNonNullChild(_1->children())));
    auto* last = find(facet, FN1(findNonNullChild(_1->children() |
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

static void UndeclaredIDNotes(UndeclaredID& issue, Symbol const* similar) {
    if (similar) {
        auto* note = issue.addNote([=](std::ostream& str) {
            str << "Did you mean \'" << formatName(*similar) << "\'?";
        });
        if (auto* nameFct = getDeclName(similar->facet()))
            note->addNote(nameFct, [=](std::ostream& str) {
                str << formatName(*similar) << " declared here";
            });
    }
}

static void TypeDefCycleNotes(TypeDefCycle& issue,
                              std::span<Symbol const* const> cycle) {
    for (auto itr = cycle.begin(); itr < cycle.end() - 1; ++itr) {
        auto* sym = *itr;
        auto fmt = [&]() -> std::function<void(std::ostream&)> {
            auto* dep = *std::next(itr);
            if (!isa<MemberSymbol>(dep) || std::next(itr) >= cycle.end() - 1)
                return [=](std::ostream& str) {
                    str << formatName(*sym) << " depends on "
                        << formatName(*dep);
                };
            ++itr;
            auto* mid = *itr;
            dep = *std::next(itr);
            if (isa<BaseClass>(mid))
                return [=](std::ostream& str) {
                    str << formatName(*sym) << " depends on "
                        << formatName(*dep) << " through inheritance";
                };
            return [=](std::ostream& str) {
                str << formatName(*sym) << " depends on " << formatName(*dep)
                    << " through member " << formatName(*mid);
            };
        };
        issue.addNote(sym->facet(), fmt());
    }
    issue.addHint([=](std::ostream& str) {
        str << "Use pointer members to break strong dependencies";
    });
}

static void AmbiguousConformanceNotes(
    AmbiguousConformance& issue, std::span<Obligation const* const> matches) {
    for (auto* sym: matches | transform(FN1(_1->symbol()))) {
        issue.addNote(sym->facet(), [=](std::ostream& str) {
            str << "Declaration matches "
                << formatDecl(sym, { .primaryQualified = true });
        });
    }
}

static void IncompleteImplNotes(IncompleteImpl& issue,
                                InterfaceLike const& interface) {
    for (auto& [key, list]: interface.obligations()) {
        for (auto* obl: list) {
            auto confs = obl->conformances();
            auto* sym = obl->symbol();
            if (confs.empty()) {
                issue.addNote(sym->facet(), [=](std::ostream& str) {
                    str << "Missing implementation for "
                        << formatDecl(sym, { .primaryQualified = true });
                });
                continue;
            }
            if (confs.size() == 1) continue;
            auto* note = issue.addNote(sym->facet(), [=](std::ostream& str) {
                str << "Multiple implementations for "
                    << formatDecl(sym, { .primaryQualified = true });
            });
            for (auto* conf: confs)
                note->addNote(conf->facet(), [=](std::ostream& str) {
                    str << "Implemented by "
                        << formatDecl(conf, { .primaryQualified = true });
                });
        }
    }
}

static void DuplicateTraitImplNotes(DuplicateTraitImpl& issue,
                                    Symbol const* existing) {
    issue.addNote(existing->Symbol::facet(), [=](std::ostream& str) {
        str << "Existing implementation is here";
    });
}

#include "Prism/Sema/SemaIssuesDef.inl"
