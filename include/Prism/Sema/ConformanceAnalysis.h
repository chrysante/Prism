#ifndef PRISM_SEMA_CONFORMANCEANALYSIS_H
#define PRISM_SEMA_CONFORMANCEANALYSIS_H

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class DiagnosticHandler;
class DependencyGraph;

/// Analyses trait and base class conformances
void analyzeConformances(MonotonicBufferResource& resource, SemaContext& ctx,
                         DiagnosticHandler& diagHandler, Target& target,
                         DependencyGraph const& dependencyGraph);

/// Analyses trait and base class conformances of a single symbol
void analyzeConformance(MonotonicBufferResource&, SemaContext& ctx,
                        DiagnosticHandler& diagHandler, Symbol& sym);

/// \Returns true of \p type conforms to \p trait
bool conformsTo(ValueType const& type, Trait const& trait);

/// \Returns true of \p derived conforms to \p base
bool conformsTo(Trait const& derived, Trait const& base);

} // namespace prism

#endif // PRISM_SEMA_CONFORMANCEANALYSIS_H
