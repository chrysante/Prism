#ifndef PRISM_SEMA_SEMADIAGNOSTIC_H
#define PRISM_SEMA_SEMADIAGNOSTIC_H

#include <utl/streammanip.hpp>

#include <Prism/Common/Diagnostic.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema/SemaFwd.h>

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

    /// Adds a descriptive note
    SemaNote* addNote(Facet const* facet, utl::vstreammanip<> impl) {
        return addNote(sourceContext(), facet, std::move(impl));
    }

    /// \overload Use this overload to provide a facet if the original
    /// diagnonistic has no source context
    SemaNote* addNote(SourceContext const* sourceContext, Facet const* facet,
                      utl::vstreammanip<> impl);

    /// \overload
    SemaNote* addNote(utl::vstreammanip<> impl) {
        return addNote(nullptr, nullptr, std::move(impl));
    }

    /// Adds a hint
    SemaHint* addHint(Facet const* facet, utl::vstreammanip<> impl) {
        return addHint(sourceContext(), facet, std::move(impl));
    }

    /// \overload
    SemaHint* addHint(SourceContext const* sourceContext, Facet const* facet,
                      utl::vstreammanip<> impl);

    /// \overload
    SemaHint* addHint(utl::vstreammanip<> impl) {
        return addHint(nullptr, nullptr, std::move(impl));
    }

protected:
    SemaDiagnostic(Diagnostic::Kind kind, SourceContext const* ctx,
                   Facet const* facet);

private:
    Facet const* fct;
};

class SemaMessage: public SemaDiagnostic {
protected:
    explicit SemaMessage(Diagnostic::Kind kind, SourceContext const* ctx,
                         Facet const* facet, utl::vstreammanip<> impl);

private:
    void header(std::ostream& os, SourceContext const* ctx) const override;

    utl::vstreammanip<> impl;
};

///
class SemaNote: public SemaMessage {
public:
    explicit SemaNote(SourceContext const* ctx, Facet const* facet,
                      utl::vstreammanip<> impl):
        SemaMessage(Diagnostic::Note, ctx, facet, std::move(impl)) {}
};

///
class SemaHint: public SemaMessage {
public:
    explicit SemaHint(SourceContext const* ctx, Facet const* facet,
                      utl::vstreammanip<> impl):
        SemaMessage(Diagnostic::Hint, ctx, facet, std::move(impl)) {}
};

} // namespace prism

#include <Prism/Sema/SemaDiagnostics.inl>

#endif // PRISM_SEMA_SEMADIAGNOSTIC_H
