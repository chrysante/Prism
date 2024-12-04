#ifndef PRISM_PARSER_PARSER_H
#define PRISM_PARSER_PARSER_H

#include <string_view>

#include <csp.hpp>

#include <Prism/Common/Allocator.h>

namespace prism {

class IssueHandler;
class SourceContext;
class AstSourceFile;
class Facet;

/// Parses a single source file and returns the constructed AST
AstSourceFile* parseSourceFile(MonotonicBufferResource& alloc,
                               SourceContext const& sourceCtx,
                               IssueHandler& iss);

/// Parses a single facet, i.e. expression or type specifier.
///
/// This function is only exposed for testing
Facet const* parseFacet(MonotonicBufferResource& alloc,
                        SourceContext const& sourceCtx, IssueHandler& iss);

} // namespace prism

#endif // PRISM_PARSER_PARSER_H
