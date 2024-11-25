#ifndef PRISM_AST_ASTDUMP_H
#define PRISM_AST_ASTDUMP_H

#include <iosfwd>

#include <Prism/Ast/AstFwd.h>

namespace prism {

class SourceContext;

/// Pretty-prints the AST rooted at \p root to \p ostream
void dumpAst(AstNode const* root, std::ostream& ostream);

} // namespace prism

#endif // PRISM_AST_ASTDUMP_H
