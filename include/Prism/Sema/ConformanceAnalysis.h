#ifndef PRISM_SEMA_CONFORMANCEANALYSIS_H
#define PRISM_SEMA_CONFORMANCEANALYSIS_H

#include <span>

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class MonotonicBufferResource;
class SemaContext;
class DependencyGraph;
struct LazySymbolInstantiation;

/// Analyses trait and base class conformances
void analyzeConformances(
    MonotonicBufferResource& resource, SemaContext& ctx, DiagnosticEmitter& DE,
    DependencyGraph const& dependencyGraph,
    std::span<LazySymbolInstantiation const> lazyInstantiations);

/// Analyses trait and base class conformances of a single symbol
void analyzeConformance(MonotonicBufferResource&, SemaContext& ctx,
                        DiagnosticEmitter& DE, Symbol& sym);

/// \Returns true of \p type conforms to \p trait
bool conformsTo(ValueType const& type, Trait const& trait);

/// \Returns true of \p derived conforms to \p base
bool conformsTo(Trait const& derived, Trait const& base);

} // namespace prism

#endif // PRISM_SEMA_CONFORMANCEANALYSIS_H
