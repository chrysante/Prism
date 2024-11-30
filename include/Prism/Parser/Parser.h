#ifndef PRISM_PARSER_PARSER_H
#define PRISM_PARSER_PARSER_H

#include <string_view>

#include <Prism/Ast/Ast.h>

namespace prism {

class IssueHandler;
class SourceContext;
class ParseTreeContext;

/// Parses a single source file and returns the constructed AST
csp::unique_ptr<AstSourceFile> parseSourceFile(SourceContext const& sourceCtx,
                                               ParseTreeContext& ptCtx,
                                               IssueHandler& iss);

} // namespace prism

#endif // PRISM_PARSER_PARSER_H
