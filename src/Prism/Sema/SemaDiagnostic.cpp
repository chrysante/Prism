#include "Prism/Sema/SemaDiagnostic.h"

#include <ostream>
#include <sstream>
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

SemaDiagnostic::SemaDiagnostic(Diagnostic::Kind kind, SourceContext const* ctx,
                               Facet const* facet):
    Diagnostic(kind, getSourceRange(facet), ctx), fct(facet) {}

SemaNote* SemaDiagnostic::addNote(SourceContext const* sourceContext,
                                  Facet const* facet,
                                  utl::vstreammanip<> impl) {
    return addChild<SemaNote>(sourceContext, facet, std::move(impl));
}

SemaHint* SemaDiagnostic::addHint(SourceContext const* sourceContext,
                                  Facet const* facet,
                                  utl::vstreammanip<> impl) {
    return addChild<SemaHint>(sourceContext, facet, std::move(impl));
}

SemaMessage::SemaMessage(Diagnostic::Kind kind, SourceContext const* ctx,
                         Facet const* facet, utl::vstreammanip<> impl):
    SemaDiagnostic(kind, ctx, facet), impl(std::move(impl)) {}

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

static std::string_view getGenCatName(GenericSymbol const* sym) {
    if (!sym) return "symbol";
    using namespace std::string_view_literals;
    // clang-format off
    return visit(*sym, csp::overload{
        [](GenStructType const&) { return "struct"sv; },
        [](GenTrait const&) { return "trait"sv; },
        [](GenTraitImpl const&) { return "impl"sv; },
        [](GenFuncImpl const&) { return "function"sv; },
    }); // clang-format on
}

static std::string numToWord(size_t num) {
    static constexpr std::string_view Words[] = { "zero",  "one",   "two",
                                                  "three", "four",  "five",
                                                  "six",   "seven", "eight",
                                                  "nine" };
    if (num < std::size(Words)) return std::string(Words[num]);
    return std::to_string(num);
}

static std::string pluralize(size_t num, std::string_view singular) {
    std::stringstream sstr;
    sstr << numToWord(num) << " ";
    if (num == 1) {
        sstr << singular;
        return std::move(sstr).str();
    }
    // Check for common pluralization cases
    if (singular.ends_with("y") &&
        !(singular.ends_with("ay") || singular.ends_with("ey") ||
          singular.ends_with("oy") || singular.ends_with("uy")))
    {
        // If it ends with 'y' preceded by a consonant, replace 'y' with 'ies'
        sstr << singular.substr(0, singular.size() - 1) << "ies";
    }
    else if (singular.ends_with("s") || singular.ends_with("x") ||
             singular.ends_with("z") || singular.ends_with("sh") ||
             singular.ends_with("ch"))
    {
        // If it ends with 's', 'x', 'z', 'sh', or 'ch', add 'es'
        sstr << singular << "es";
    }
    else {
        // Default case, just add 's'
        sstr << singular << "s";
    }
    return std::move(sstr).str();
}

static void UndeclaredIDNotes(UndeclaredID& diag, Symbol const* similar) {
    if (similar) {
        auto* note = diag.addNote([=](std::ostream& str) {
            str << "Did you mean \'" << formatName(*similar) << "\'?";
        });
        if (auto* nameFct = getDeclName(similar->facet()))
            note->addNote(nameFct, [=](std::ostream& str) {
                str << formatName(*similar) << " declared here";
            });
    }
}

static void TypeDefCycleNotes(TypeDefCycle& diag,
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
        diag.addNote(getSourceContext(sym), sym->facet(), fmt());
    }
    diag.addHint([=](std::ostream& str) {
        str << "Use pointer members to break strong dependencies";
    });
}

static void AmbiguousConformanceNotes(
    AmbiguousConformance& diag, std::span<Obligation const* const> matches) {
    for (auto* sym: matches | transform(FN1(_1->symbol()))) {
        diag.addNote(sym->facet(), [=](std::ostream& str) {
            str << "Declaration matches "
                << formatDecl(sym, { .primaryQualified = true });
        });
    }
}

static void IncompleteImplNotes(IncompleteImpl& diag,
                                InterfaceLike const& interface) {
    for (auto& [key, list]: interface.obligations()) {
        for (auto* obl: list) {
            auto confs = obl->conformances();
            auto* sym = obl->symbol();
            if (confs.empty()) {
                diag.addNote(sym->facet(), [=](std::ostream& str) {
                    str << "Missing implementation for "
                        << formatDecl(sym, { .primaryQualified = true });
                });
                continue;
            }
            if (confs.size() == 1) continue;
            auto* note = diag.addNote(sym->facet(), [=](std::ostream& str) {
                str << "Multiple implementations for "
                    << formatDecl(sym, { .primaryQualified = true })
                    << " must be resolved";
            });
            for (auto* conf: confs)
                note->addNote(conf->facet(), [=](std::ostream& str) {
                    str << "Implemented by "
                        << formatDecl(conf, { .primaryQualified = true });
                });
        }
    }
}

static void DuplicateTraitImplNotes(DuplicateTraitImpl& diag,
                                    Symbol const* existing) {
    diag.addNote(existing->Symbol::facet(), [=](std::ostream& str) {
        str << "Existing implementation defined here";
    });
}

#include "Prism/Sema/SemaDiagnosticsDef.inl"
