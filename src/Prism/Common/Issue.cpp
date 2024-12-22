#include "Prism/Common/Issue.h"

#include <ostream>

#include "Prism/Source/SourceContext.h"

using namespace prism;

void Issue::format(std::ostream& str, SourceContext const& ctx) const {
    auto loc = ctx.getSourceLocation(sourceIndex());
    str << "Error: [L:" << loc.line + 1 << " C:" << loc.column + 1 << "] ";
    doFormat(str, ctx);
    str << "\n";
}
