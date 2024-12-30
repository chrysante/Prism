#include "Prism/Common/DiagnosticHandler.h"

#include <iostream>

#include <Prism/Source/SourceContext.h>

using namespace prism;

void DiagnosticHandler::print(SourceContext const& ctx) {
    format(std::cerr, ctx);
}

void DiagnosticHandler::format(std::ostream& str,
                               SourceContext const& ctx) const {
    for (auto& issue: *this) {
        issue.format(str, &ctx);
    }
}
