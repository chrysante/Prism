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

static Facet const* get_def_facet(Symbol const* function) {
    auto* def = [&] {
        if (auto* inst = dyncast<FunctionInst const*>(function))
            return inst->definition();
        return dyncast<FunctionDef const*>(function);
    }();
    if (!def) return nullptr;
    return def->facet();
}

static std::unique_ptr<AmbiguousCall> make_ambi_err(
    Facet const* call_facet, std::string name,
    std::span<Function const* const> candidates) {
    auto err = std::make_unique<AmbiguousCall>(call_facet, name);
    for (auto* candidate: candidates) {
        auto* facet = get_def_facet(candidate);
        err->add_note(facet,
                      [=](std::ostream& str) { str << "possible candidate"; });
    }
    return err;
}

static std::unique_ptr<NoMatchingFunction> make_no_match_err(
    Facet const* call_facet, std::string name,
    std::span<Symbol* const> overload_set) {
    auto err = std::make_unique<NoMatchingFunction>(call_facet, name);
    for (auto* function: overload_set) {
        auto* facet = get_def_facet(function);
        err->add_note(facet, [=](std::ostream& str) { str << "not a match"; });
    }
    return err;
}

ORResult prism::resolve_overload(SemaContext& ctx,
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
        return utl::unexpected(make_ambi_err(call_facet, name, candidates));
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
        return utl::unexpected(make_ambi_err(call_facet, name, candidates));
    return utl::unexpected(make_no_match_err(call_facet, name, overload_set));
}
