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
    for (auto& msg: messages) {
        FmtImpl::formatMessage(msg, str, ctx);
    }
}

void Issue::message(MessageKind kind, std::function<void(std::ostream&)> fmt) {
    message(kind, index, std::move(fmt));
}

void Issue::message(MessageKind kind, uint32_t sourceIndex,
                    std::function<void(std::ostream&)> fmt) {
    messages.emplace_back(kind, sourceIndex, std::move(fmt));
}
