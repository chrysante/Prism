#ifndef PRISM_SEMA2_OVERLOADRESOLUTION_H
#define PRISM_SEMA2_OVERLOADRESOLUTION_H

#include <span>

#include <utl/expected.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SemaDiagnostic;
class SourceContext;
class Facet;

/// Deduces generic arguments for \p generic from \p arguments and instantiates
/// the function in \p ctx
FunctionInst* deduce_generic_function(SemaContext& ctx, FunctionDef* generic,
                                      std::span<Value const* const> arguments);

using ORResult = utl::expected<Function*, std::unique_ptr<SemaDiagnostic>>;

/// Resolves the best match from \p overload_set for \p arguments
/// Instantiates generic functions if necessary.
ORResult resolve_overload(SemaContext& ctx, SourceContext const* source_context,
                          Facet const* call_facet, std::string name,
                          std::span<Symbol* const> overload_set,
                          std::span<Value const* const> arguments);

} // namespace prism

#endif // PRISM_SEMA2_OVERLOADRESOLUTION_H
