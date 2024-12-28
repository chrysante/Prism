#include "Prism/Common/IssueHandler.h"

#include <iostream>

#include <Prism/Source/SourceContext.h>

using namespace prism;

void IssueHandler::print(SourceContext const& ctx) { format(std::cerr, ctx); }

void IssueHandler::format(std::ostream& str, SourceContext const& ctx) const {
    for (auto& issue: *this) {
        issue.format(str, &ctx);
    }
}
