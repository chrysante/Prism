#ifndef PRISM_DIAGNOSTIC_DIAGNOSTICEMITTER_H
#define PRISM_DIAGNOSTIC_DIAGNOSTICEMITTER_H

#include <concepts>
#include <memory>
#include <vector>

namespace prism {

class Diagnostic;

/// \class DiagnosticEmitter
/// \brief Interface for emitting diagnostics.
///
/// This abstract class defines the interface for emitting diagnostic messages
/// such as warnings and errors. Derived classes must implement the methods
/// to handle the emission and management of diagnostics.
class DiagnosticEmitter {
public:
    virtual ~DiagnosticEmitter() = default;

    /// Emits a diagnostic
    virtual Diagnostic* emit(std::unique_ptr<Diagnostic> diag) = 0;

    /// \overload for derived types
    template <std::derived_from<Diagnostic> I, typename... Args>
        requires std::constructible_from<I, Args&&...>
    I* emit(Args&&... args) {
        auto* p = emit(std::make_unique<I>(std::forward<Args>(args)...));
        return static_cast<I*>(p);
    }

    /// \Returns true if any of the pushed diagnostics are errors
    virtual bool hasErrors() const = 0;

    /// \Returns true if no diagnostics have been emitted
    virtual bool empty() const = 0;

    /// Deletes all emitted diagnostics
    virtual void clear() = 0;

    virtual std::vector<Diagnostic const*> getAll() const = 0;
};

/// Creates a default diagnostic emitter
std::unique_ptr<DiagnosticEmitter> makeDefaultDiagnosticEmitter();

/// Creates a trapping diagnostic emitter.
/// This emitter traps on emitted diagnostics
std::unique_ptr<DiagnosticEmitter> makeTrappingDiagnosticEmitter();

} // namespace prism

#endif // PRISM_DIAGNOSTIC_DIAGNOSTICEMITTER_H
