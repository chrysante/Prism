#ifndef PRISM_SEMA_CONSTRUCTION_H
#define PRISM_SEMA_CONSTRUCTION_H

#include <span>

#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class DiagnosticEmitter;
class SourceContext;
class SemaContext;

struct SourceFilePair {
    SourceFileFacet const* facet;
    SourceContext const* context;
};

Module* construct_sema_ir(SemaContext& context, DiagnosticEmitter& DE,
                          std::span<SourceFilePair const> sources);

} // namespace prism

#endif // PRISM_SEMA_CONSTRUCTION_H
