#ifndef PRISM_SEMA_SEMAISSUE_H
#define PRISM_SEMA_SEMAISSUE_H

#include <utl/streammanip.hpp>

#include <Prism/Common/Issue.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaNote;
class SemaHint;

/// Base class of a sema issues
class SemaIssue: public Issue {
public:
    /// \Returns the source facet where the issue occured
    Facet const* facet() const { return fct; }

    /// \overload
    template <std::derived_from<Facet> F>
    F const* facet() const {
        return dyncast<F const*>(facet());
    }

    ///
    SourceContext const* sourceContext() const { return ctx; }

    /// Adds a descriptive note
    SemaNote* addNote(Facet const* facet, utl::vstreammanip<> impl);

    /// \overload
    SemaNote* addNote(utl::vstreammanip<> impl) {
        return addNote(nullptr, std::move(impl));
    }

    /// Adds a hint
    SemaHint* addHint(Facet const* facet, utl::vstreammanip<> impl);

    /// \overload
    SemaHint* addHint(utl::vstreammanip<> impl) {
        return addHint(nullptr, std::move(impl));
    }

protected:
    SemaIssue(Issue::Kind kind, SourceContext const* ctx, Facet const* facet);

private:
    SourceContext const* ctx;
    Facet const* fct;
};

class SemaMessage: public SemaIssue {
protected:
    explicit SemaMessage(Issue::Kind kind, SourceContext const* ctx,
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
        SemaMessage(Issue::Note, ctx, facet, std::move(impl)) {}
};

///
class SemaHint: public SemaMessage {
public:
    explicit SemaHint(SourceContext const* ctx, Facet const* facet,
                      utl::vstreammanip<> impl):
        SemaMessage(Issue::Hint, ctx, facet, std::move(impl)) {}
};

} // namespace prism

#include <Prism/Sema/SemaIssues.inl>

#endif // PRISM_SEMA_SEMAISSUE_H
