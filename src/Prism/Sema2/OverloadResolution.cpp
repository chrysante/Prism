#include "Prism/Sema2/OverloadResolution.h"

#include <ostream>

#include <range/v3/algorithm.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/GenericMatching.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/SemaDiagnostic.h"
#include "Prism/Sema2/SubContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

// FIXME: remove this
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

static std::pair<Facet const*, SourceContext const*> get_def_facet_and_ctx(
    Symbol const* function) {
    auto* def = [&] {
        if (auto* inst = dyncast<FunctionInst const*>(function))
            return inst->definition();
        return dyncast<FunctionDef const*>(function);
    }();
    if (!def) return { nullptr, nullptr };
    return { def->facet(), get_source_context(def) };
}

static std::unique_ptr<AmbiguousCall> make_ambi_err(
    SourceContext const* source_context, Facet const* call_facet,
    std::string name, std::span<Function const* const> candidates) {
    auto err =
        std::make_unique<AmbiguousCall>(source_context, call_facet, name);
    for (auto* candidate: candidates) {
        auto [facet, src_ctx] = get_def_facet_and_ctx(candidate);
        err->add_note(src_ctx, facet,
                      [=](std::ostream& str) { str << "possible candidate"; });
    }
    return err;
}

static std::unique_ptr<NoMatchingFunction> make_no_match_err(
    SourceContext const* source_context, Facet const* call_facet,
    std::string name, std::span<Symbol* const> overload_set) {
    auto err =
        std::make_unique<NoMatchingFunction>(source_context, call_facet, name);
    for (auto* function: overload_set) {
        auto [facet, src_ctx] = get_def_facet_and_ctx(function);
        err->add_note(src_ctx, facet,
                      [=](std::ostream& str) { str << "not a match"; });
    }
    return err;
}

ORResult prism::resolve_overload(SemaContext& ctx,
                                 SourceContext const* source_context,
                                 SubContext const& sub_context,
                                 Facet const* call_facet, std::string name,
                                 std::span<Symbol* const> overload_set,
                                 std::span<Value const* const> arguments) {
    utl::small_vector<Function*> candidates;
    utl::small_vector<FunctionDef*> generics;
    for (auto* sym: overload_set) {
        if (auto* function = dyncast<Function*>(sym)) {
            bool match = ranges::equal(function->arguments(), arguments,
                                       ranges::equal_to{}, FN1(, _1.type()),
                                       FN1(, _1->type()));
            if (match) candidates.push_back(function);
        }
        else {
            generics.push_back(cast<FunctionDef*>(sym));
        }
    }
    if (candidates.size() == 1) return candidates.front();
    if (candidates.size() > 1)
        return utl::unexpected(
            make_ambi_err(source_context, call_facet, name, candidates));
    for (auto* generic: generics) {
        auto deduced_sub_context =
            deduce_generic_args(sub_context, *generic, arguments);
        if (deduced_sub_context) {
            auto* function =
                ctx.get_function_instantiation(*deduced_sub_context, generic);
            candidates.push_back(function);
        }
    }
    if (candidates.size() == 1) return candidates.front();
    if (candidates.size() > 1)
        return utl::unexpected(
            make_ambi_err(source_context, call_facet, name, candidates));
    return utl::unexpected(
        make_no_match_err(source_context, call_facet, name, overload_set));
}
