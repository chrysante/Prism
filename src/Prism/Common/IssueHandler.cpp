#include "Prism/Common/IssueHandler.h"

#include <ostream>

#include <Prism/Source/SourceContext.h>

using namespace prism;

void IssueHandler::format(std::ostream& str, SourceContext const& ctx) const {
    for (auto& issue: *this) {
        issue.format(str, ctx);
    }
}
