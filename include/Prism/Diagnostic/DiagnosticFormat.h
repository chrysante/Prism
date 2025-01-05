#ifndef PRISM_DIAGNOSTIC_DIAGNOSTICFORMAT_H
#define PRISM_DIAGNOSTIC_DIAGNOSTICFORMAT_H

#include <iosfwd>

namespace prism {

class DiagnosticEmitter;

/// Writes all diagnostics to `std::cerr`
void print(DiagnosticEmitter const& emitter);

/// Writes all issues formatted to \p os
void print(DiagnosticEmitter const& emitter, std::ostream& os);

} // namespace prism

#endif // PRISM_DIAGNOSTIC_DIAGNOSTICFORMAT_H
