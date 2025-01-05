#ifndef PRISM_PARSER_PARSER_H
#define PRISM_PARSER_PARSER_H

#include <string_view>

#include <Prism/Common/Allocator.h>
#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Facet/FacetFwd.h>

namespace prism {

class SourceContext;

/// Parses a single source file and returns the constructed source file facet
SourceFileFacet const* parseSourceFile(MonotonicBufferResource& alloc,
                                       SourceContext const& sourceCtx,
                                       DiagnosticEmitter& DE);

/// Parses a single expression facet.
/// This function is only exposed for testing
Facet const* parseExpr(MonotonicBufferResource& alloc,
                       SourceContext const& sourceCtx, DiagnosticEmitter& DE);

/// This function is only exposed for testing
Facet const* parseTypeSpec(MonotonicBufferResource& alloc,
                           SourceContext const& sourceCtx,
                           DiagnosticEmitter& DE);

} // namespace prism

#endif // PRISM_PARSER_PARSER_H
