#include "Prism/Sema2/SemaDiagnostic.h"

#include <ostream>
#include <sstream>
#include <string_view>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Functional.h"
#include "Prism/Common/PrettyName.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
// #include "Prism/Sema/Contracts.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaPrint.h"
#include "Prism/Sema2/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::concat;
using ranges::views::join;
using ranges::views::transform;
using ranges::views::values;

static SourceContext const* get_source_context(Symbol const* sym) {
    if (!sym) return nullptr;
    auto* scope = sym->parent_scope();
    while (scope) {
        if (auto* sourceFile =
                dyncast<SourceFile const*>(scope->defining_symbol()))
            return &sourceFile->source_context();
        scope = scope->parent_scope();
    }
    return nullptr;
}

static Facet const* find_non_null_child(auto&& children) {
    auto itr = ranges::find_if(children, ToAddress);
    PRISM_ASSERT(itr != children.end());
    return *itr;
};

static std::optional<SourceRange> get_source_range(Facet const* facet) {
    if (!facet) return std::nullopt;
    auto find = [](Facet const* facet, auto inc) {
        while (!isa<TerminalFacet>(facet)) {
            PRISM_ASSERT(facet);
            facet = inc(facet);
        }
        return cast<TerminalFacet const*>(facet);
    };
    auto* first = find(facet, FN1(find_non_null_child(_1->children())));
    auto* last = find(facet, FN1(find_non_null_child(_1->children() |
                                                     ranges::views::reverse)));
    uint32_t begin_index = first->token().index;
    uint32_t end_index = last->token().index + last->token().sourceLen;
    return SourceRange{ begin_index, end_index - begin_index };
}

SemaDiagnostic::SemaDiagnostic(Diagnostic::Kind kind, SourceContext const* ctx,
                               Facet const* facet):
    Diagnostic(kind, get_source_range(facet), ctx), fct(facet) {}

SemaNote* SemaDiagnostic::add_note(SourceContext const* source_context,
                                   Facet const* facet,
                                   utl::vstreammanip<> impl) {
    return addChild<SemaNote>(source_context, facet, std::move(impl));
}

SemaHint* SemaDiagnostic::add_hint(SourceContext const* source_context,
                                   Facet const* facet,
                                   utl::vstreammanip<> impl) {
    return addChild<SemaHint>(source_context, facet, std::move(impl));
}

SemaMessage::SemaMessage(Diagnostic::Kind kind, SourceContext const* ctx,
                         Facet const* facet, utl::vstreammanip<> impl):
    SemaDiagnostic(kind, ctx, facet), impl(std::move(impl)) {}

void SemaMessage::header(std::ostream& str, SourceContext const*) const {
    str << impl;
}

static std::string_view get_id(Facet const* facet, SourceContext const* ctx) {
    if (!ctx) return {};
    auto* term = dyncast<TerminalFacet const*>(facet);
    if (!term || term->token().kind != TokenKind::Identifier) return {};
    return ctx->getTokenStr(term->token());
}

static Facet const* get_decl_name(Facet const* facet) {
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

static std::string_view get_gen_symbol_category_name(DeclSymbol const* sym) {
    if (!sym) return "symbol";
    using namespace std::string_view_literals;
    // clang-format off
    return visit(*sym, csp::overload{
        [](StructDef const&) { return "struct"sv; },
        [](TraitDef const&) { return "trait"sv; },
        [](TraitImplDef const&) { return "impl"sv; },
        [](FunctionDef const&) { return "function"sv; },
        [](BindingDef const&) { return "binding"sv; },
    }); // clang-format on
}

static std::string num_to_word(size_t num) {
    static constexpr std::string_view Words[] = { "zero",  "one",   "two",
                                                  "three", "four",  "five",
                                                  "six",   "seven", "eight",
                                                  "nine" };
    if (num < std::size(Words)) return std::string(Words[num]);
    return std::to_string(num);
}

static std::string pluralize(size_t num, std::string_view singular) {
    std::stringstream sstr;
    sstr << num_to_word(num) << " ";
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

static void indeclared_id_notes(UndeclaredID& diag, Symbol const* similar) {
    if (!similar) return;
    auto* note = diag.add_note([=](std::ostream& str) {
        str << "Did you mean \'" << format_name(*similar) << "\'?";
    });
    if (auto* name_facet = get_decl_name(similar->facet()))
        note->add_note(get_source_context(similar), name_facet,
                       [=](std::ostream& str) {
            str << format_name(*similar) << " declared here";
        });
}

static void typedef_cycle_notes(TypeDefCycle& diag,
                                std::span<Symbol const* const> cycle) {
    for (auto itr = cycle.begin(); itr < cycle.end() - 1; ++itr) {
        auto* sym = *itr;
        auto fmt = [&]() -> std::function<void(std::ostream&)> {
            auto* dep = *std::next(itr);
#if 0
            if (!isa<MemberSymbol>(dep) || std::next(itr) >= cycle.end() - 1)
                return [=](std::ostream& str) {
                    str << format_name(*sym) << " depends on "
                        << format_name(*dep);
                };
#endif
            ++itr;
            auto* mid = *itr;
            dep = *std::next(itr);
#if 0
            if (isa<BaseClass>(mid))
                return [=](std::ostream& str) {
                    str << format_name(*sym) << " depends on "
                        << format_name(*dep) << " through inheritance";
                };
#endif
            return [=](std::ostream& str) {
                str << format_name(*sym) << " depends on " << format_name(*dep)
                    << " through member " << format_name(*mid);
            };
        };
        diag.add_note(get_source_context(sym), sym->facet(), fmt());
    }
    diag.add_hint([=](std::ostream& str) {
        str << "Use pointer members to break strong dependencies";
    });
}

#if 0
static void IncompleteImplNotes(IncompleteImpl& diag,
                                InterfaceLike const& interface) {
    auto obligations = concat(interface.typeObligations() | values | join |
                                  transform(cast<Obligation const*>),
                              interface.funcObligations() | values | join);
    for (auto obl: obligations) {
        auto confs = obl->conformances();
        auto* sym = obl->symbol();
        if (confs.empty()) {
            diag.add_note(sym->facet(), [=](std::ostream& str) {
                str << "Missing implementation for "
                    << format_decl(sym, { .primaryQualified = true });
            });
            continue;
        }
        if (confs.size() == 1) continue;
        auto* note = diag.add_note(sym->facet(), [=](std::ostream& str) {
            str << "Multiple implementations for "
                << format_decl(sym, { .primaryQualified = true })
                << " must be resolved";
        });
        for (auto* conf: confs)
            note->add_note(conf->facet(), [=](std::ostream& str) {
                str << "Implemented by "
                    << format_decl(conf, { .primaryQualified = true });
            });
    }
}

static void DuplicateTraitImplNotes(DuplicateTraitImpl& diag,
                                    Symbol const* existing) {
    diag.add_note(existing->Symbol::facet(), [=](std::ostream& str) {
        str << "Existing implementation defined here";
    });
}
#endif

#include "Prism/Sema2/SemaDiagnosticsDef.inl"
