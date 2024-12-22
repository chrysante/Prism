#ifndef PRISM_SEMA_SEMAISSUE_H
#define PRISM_SEMA_SEMAISSUE_H

#include <Prism/Common/Issue.h>
#include <Prism/Facet/FacetFwd.h>

namespace prism {

/// Base class of a sema issues
class SemaIssue: public Issue {
public:
    Facet const* facet() const { return fct; }

    template <std::derived_from<Facet> F>
    F const* facet() const {
        return dyncast<F const*>(facet());
    }

    SourceContext const& sourceContext() const { return ctx; }

protected:
    SemaIssue(Issue::Kind kind, SourceContext const& ctx, Facet const* facet);

private:
    SourceContext const& ctx;
    Facet const* fct;
};

} // namespace prism

#include <Prism/Sema/SemaIssues.inl>

#endif // PRISM_SEMA_SEMAISSUE_H
