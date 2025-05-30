//===----------------------------------------------------------------------===//
//
// Overload Resolution for Function Calls
//
// The resolution algorithm works as follows:
//
// 1. Candidate Collection
//    - Iterate through all symbols in the overload set.
//    - If the symbol is a non-generic `Function`, check for type equality
//      against the call arguments.
//    - If the symbol is a generic `FunctionDef`, attempt to deduce type
//      arguments via `deduce_generic_args`. If deduction succeeds, record the
//      resulting instantiation as a "lazy" candidate.
//
// 2. Candidate Scoring
//    - Each candidate is assigned a `score_vec`, which represents how well
//      each argument matched:
//        * Exact matches receive a high score (INT_MAX).
//        * Generic deductions receive scores based on how specific the match
//          was.
//    - Candidates are sorted by lexicographically comparing their `score_vec`
//      values in descending order.
//
// 3. Best Match Selection
//    - The top candidates with the highest score are considered best matches.
//    - If there is a unique best match, it is selected.
//    - If multiple candidates share the highest score, the call is considered
//      ambiguous and an `AmbiguousCall` error is returned.
//    - If no candidates match, a `NoMatchingFunction` error is returned.
//
//===----------------------------------------------------------------------===//

#include "Prism/Sema/OverloadResolution.h"

#include <ostream>
#include <variant>

#include <range/v3/algorithm.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/GenericMatching.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/SubContext.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

using ranges::views::drop;

namespace {

struct LazyInst {
    SubContext sub_context;
    FunctionDef* definition;
};

struct Candidate {
    std::variant<Function*, LazyInst> function;
    utl::small_vector<int> score_vec;

    static Candidate make(Function* function) {
        return { .function = function,
                 .score_vec = utl::small_vector<int>(function->num_arguments(),
                                                     INT_MAX) };
    }
};

} // namespace

static Facet const* get_def_facet(Symbol const* sym) {
    auto* def = [&] {
        if (auto* inst = dyncast<FunctionInst const*>(sym))
            return inst->definition();
        return dyncast<FunctionDef const*>(sym);
    }();
    if (!def) return nullptr;
    return def->facet();
}

static std::unique_ptr<AmbiguousCall> make_ambi_err(
    Facet const* call_facet, std::string name,
    std::span<Candidate const> candidates) {
    auto err = std::make_unique<AmbiguousCall>(call_facet, name);
    for (auto& candidate: candidates) {
        // clang-format off
        auto* facet = std::visit(csp::overload{
            [](LazyInst const& lazy) { return get_def_facet(lazy.definition); },
            [](Function const* function) { return get_def_facet(function); },
        }, candidate.function); // clang-format on
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
    utl::small_vector<Candidate> candidates;
    for (auto* sym: overload_set) {
        if (auto* function = dyncast<Function*>(sym)) {
            bool match = ranges::equal(function->arguments(), arguments,
                                       ranges::equal_to{}, FN1(, _1.type()),
                                       FN1(, _1->type()));
            if (!match) continue;
            candidates.push_back(Candidate::make(function));
        }
        else if (auto* generic = dyncast<FunctionDef*>(sym)) {
            auto deduction_result =
                deduce_generic_args(sub_context, *generic, arguments);
            if (!deduction_result.success) continue;
            candidates.push_back(
                { .function = LazyInst{ std::move(deduction_result.sub_context),
                                        generic },
                  .score_vec = std::move(deduction_result.score_vec) });
        }
    }
    // Higher scores (i.e., better matches) come first
    ranges::sort(candidates, [](Candidate const& a, Candidate const& b) {
        return ranges::lexicographical_compare(a.score_vec, b.score_vec,
                                               ranges::greater{});
    });
    if (candidates.empty())
        return utl::unexpected(
            make_no_match_err(call_facet, name, overload_set));
    size_t num_best_matches = 1;
    for (auto& candidate: candidates | drop(1)) {
        if (candidate.score_vec != candidates.front().score_vec) break;
        ++num_best_matches;
    }
    std::span best_matches(candidates.data(), num_best_matches);
    if (best_matches.size() > 1)
        return utl::unexpected(make_ambi_err(call_facet, name, best_matches));
    // clang-format off
    return std::visit(csp::overload{
        [&](LazyInst const& lazy) -> Function* {
            return ctx.get_function_instantiation(lazy.sub_context,
                                                  lazy.definition);
        },
        [](Function* function) { return function; },
    }, best_matches.front().function); // clang-format on
}
