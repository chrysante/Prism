#ifndef PRISM_SEMA2_GENERICMATCHING_H
#define PRISM_SEMA2_GENERICMATCHING_H

#include <optional>
#include <span>

#include <utl/function_view.hpp>
#include <utl/type_traits.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema2/AnalysisContext.h>
#include <Prism/Sema2/SemaFwd.h>
#include <Prism/Sema2/SubContext.h>

namespace prism {

class SubContext;

namespace detail {

bool is_any_null(auto*... args) { return (!args || ...); }

bool all_equal(auto* first, auto*... rest) { return ((first == rest) && ...); }

template <typename... S>
static decltype(auto) match_generic_impl(auto&& matcher,
                                         std::type_identity_t<S>*... symbols) {
    ((symbols = canonicalize(symbols)), ...);
    if (is_any_null(symbols...)) return matcher.null_fallback();
    if constexpr (sizeof...(S) > 1)
        if (all_equal(symbols...)) return matcher.exact_match();
    // For compound types, recur on inner structure
    // clang-format off
    return visit(*symbols..., csp::overload{
        [&](utl::copy_cv_t<S, StructInst>&... struct_insts) {
            return matcher.structural(struct_insts...);
        },
        [&](utl::copy_cv_t<S, Symbol>&... symbols) {
            return matcher.base_case(symbols...);
        }
    }); // clang-format on
}

} // namespace detail

/// Recursively process a pack of `Symbol` pointers.
///
/// Requires `matcher` to provide:
/// - `null_fallback()`: called if any input is null
/// - `exact_match()`: called if all inputs are pointer-equal (only when 2 or
/// more symbols are provided)
/// - `structural(StructInst&...)`: called when all inputs are StructInsts
/// - `base_case(Symbol&...)`: called for all other symbol combinations
template <std::derived_from<Symbol>... S>
static decltype(auto) match_generic(auto&& matcher, S*... symbols) {
    return detail::match_generic_impl<utl::copy_cv_t<S, Symbol>...>(matcher,
                                                                    symbols...);
}

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
