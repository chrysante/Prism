#ifndef PRISM_PARSER_PARSER_H
#define PRISM_PARSER_PARSER_H

#include <string_view>

#include <Prism/Ast/Ast.h>

namespace prism {

class IssueHandler;

csp::unique_ptr<AstSourceFile> parseSourceFile(std::string_view source,
                                               IssueHandler& iss);

} // namespace prism

#endif // PRISM_PARSER_PARSER_H
