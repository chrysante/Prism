#include "Prism/Common/Issue.h"

#include <ostream>

#include "Prism/Source/SourceContext.h"

using namespace prism;

namespace prism {

struct FmtImpl {
    static void formatMessage(SourceMessage const& msg, std::ostream& str,
                              SourceContext const& ctx) {
        auto loc = ctx.getSourceLocation(msg.sourceIndex());
        str << "Error: [L:" << loc.line + 1 << " C:" << loc.column + 1 << "] ";
        msg.fmt(str);
        str << "\n";
    }
};

} // namespace prism

void Issue::format(std::ostream& str, SourceContext const& ctx) const {
    FmtImpl::formatMessage(msg, str, ctx);
}

void Issue::message(std::function<void(std::ostream&)> fmt) {
    message(index, std::move(fmt));
}

void Issue::message(uint32_t sourceIndex,
                    std::function<void(std::ostream&)> fmt) {
    msg = { sourceIndex, std::move(fmt) };
}
