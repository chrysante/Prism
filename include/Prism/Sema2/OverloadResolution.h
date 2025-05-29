#ifndef PRISM_SEMA2_OVERLOADRESOLUTION_H
#define PRISM_SEMA2_OVERLOADRESOLUTION_H

#include <memory>
#include <span>

#include <utl/expected.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SemaDiagnostic;
class SourceContext;
class Facet;

using ORResult = utl::expected<Function*, std::unique_ptr<SemaDiagnostic>>;

/// Resolves the best match from \p overload_set for \p arguments
/// Instantiates generic functions if necessary.
ORResult resolve_overload(SemaContext& ctx, SubContext const& sub_context,
                          Facet const* call_facet, std::string name,
                          std::span<Symbol* const> overload_set,
                          std::span<Value const* const> arguments);

} // namespace prism

#endif // PRISM_SEMA2_OVERLOADRESOLUTION_H
