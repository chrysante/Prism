#ifndef PRISM_PARSER_PARSER_H
#define PRISM_PARSER_PARSER_H

#include <string_view>

#include <Prism/Ast/Ast.h>
#include <Prism/Common/Allocator.h>

namespace prism {

class IssueHandler;
class SourceContext;

/// Parses a single source file and returns the constructed AST
csp::unique_ptr<AstSourceFile> parseSourceFile(MonotonicBufferAllocator& alloc,
                                               SourceContext const& sourceCtx,
                                               IssueHandler& iss);

} // namespace prism

#endif // PRISM_PARSER_PARSER_H
