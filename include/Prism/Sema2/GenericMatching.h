#ifndef PRISM_SEMA2_GENERICMATCHING_H
#define PRISM_SEMA2_GENERICMATCHING_H

#include <optional>
#include <span>

#include <utl/function_view.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SubContext;

///
bool match_generic(Symbol const* param_sym, Symbol const* arg_sym,
                   utl::function_view<bool(GenParamBase const&, Symbol const&)>
                       compare_gen_param);

/// Deduces generic arguments for \p generic from \p call_arguments
/// \Returns a substitution context for instantiating \p generic or empty
/// optional on failure.
std::optional<SubContext> deduce_generic_args(
    SubContext const& sub_context, FunctionDef& generic,
    std::span<Value const* const> call_arguments);

} // namespace prism

#endif // PRISM_SEMA2_GENERICMATCHING_H
