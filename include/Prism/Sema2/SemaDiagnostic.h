#ifndef PRISM_SEMA2_SEMADIAGNOSTIC_H
#define PRISM_SEMA2_SEMADIAGNOSTIC_H

#include <utl/streammanip.hpp>

#include <Prism/Diagnostic/Diagnostic.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SemaNote;
class SemaHint;

/// Base class of all sema diagnostics
class SemaDiagnostic: public Diagnostic {
public:
    /// \Returns the source facet where the diag occured
    Facet const* facet() const { return fct; }

    /// \overload
    template <std::derived_from<Facet> F>
    F const* facet() const {
        return dyncast<F const*>(facet());
    }

    /// \overload Use this overload to provide a facet if the original
    /// diagnonistic has no source context
    SemaNote* add_note(Facet const* facet, utl::vstreammanip<> impl);

    /// \overload
    SemaNote* add_note(utl::vstreammanip<> impl) {
        return add_note(nullptr, std::move(impl));
    }

    /// Adds a hint
    SemaHint* add_hint(Facet const* facet, utl::vstreammanip<> impl);

    /// \overload
    SemaHint* add_hint(utl::vstreammanip<> impl) {
        return add_hint(nullptr, std::move(impl));
    }

protected:
    SemaDiagnostic(Diagnostic::Kind kind, Facet const* facet);

private:
    Facet const* fct;
};

///
class SemaMessage: public SemaDiagnostic {
protected:
    explicit SemaMessage(Diagnostic::Kind kind, Facet const* facet,
                         utl::vstreammanip<> impl);

private:
    void header(std::ostream& os, SourceContext const* ctx) const override;

    utl::vstreammanip<> impl;
};

///
class SemaNote: public SemaMessage {
public:
    explicit SemaNote(Facet const* facet, utl::vstreammanip<> impl):
        SemaMessage(Diagnostic::Note, facet, std::move(impl)) {}
};

///
class SemaHint: public SemaMessage {
public:
    explicit SemaHint(Facet const* facet, utl::vstreammanip<> impl):
        SemaMessage(Diagnostic::Hint, facet, std::move(impl)) {}
};

} // namespace prism

#include <Prism/Sema2/SemaDiagnosticsDecl.inl>

#endif // PRISM_SEMA2_SEMADIAGNOSTIC_H
