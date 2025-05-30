#ifndef PRISM_SEMA2_GENERICMATCHING_H
#define PRISM_SEMA2_GENERICMATCHING_H

#include <optional>
#include <span>

#include <utl/function_view.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema2/SemaFwd.h>
#include <Prism/Sema2/SubContext.h>

namespace prism {

class SubContext;

///
bool match_generic(Symbol const* param_sym, Symbol const* arg_sym,
                   utl::function_view<bool(GenParamBase const&, Symbol const&)>
                       compare_gen_param);

/// Result structure for `deduce_generic_args()`
struct GenericDeductionResult {
    bool success;

    /// Score vector reporting how 'specific' the match was for each argument.
    /// Empty if `success == false`, otherwise contains a positive value for
    /// each argument. Higher values indicate better matches. Exact matches have
    /// value `INT_MAX`, otherwise the value is the recursion depth encountered
    /// during structural matching.
    utl::small_vector<int> score_vec = {};

    /// The deduced generic arguments
    SubContext sub_context = {};
};

/// Deduces generic arguments for \p generic from \p call_arguments
GenericDeductionResult deduce_generic_args(
    SubContext const& sub_context, FunctionDef& generic,
    std::span<Value const* const> call_arguments);

} // namespace prism

#endif // PRISM_SEMA2_GENERICMATCHING_H
