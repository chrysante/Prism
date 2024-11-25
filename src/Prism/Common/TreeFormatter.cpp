#include "Prism/Common/TreeFormatter.h"

#include <termfmt/termfmt.h>

using namespace prism;
using enum TreeFormatter::Level;

#if 0
static constexpr std::string_view Repr[] = { "+- ", "|  ", "+- ", "   " };
#else
static constexpr std::string_view Repr[] = { "├╴ ", "│  ", "╰─ ", "   " };
#endif
static constexpr TreeFormatter::Level Next[] = { ChildContinue, ChildContinue,
                                                 LastChildContinue,
                                                 LastChildContinue };

void TreeFormatter::Indenter::operator()(std::streambuf* buf) const {
    // Can't use the global modifier stack here, because the indentation may be
    // written after users pushed modifiers that shall only appear after the
    // indentation
    fmt->ostr << (tfmt::BrightGrey | tfmt::Bold);
    for (auto& level: fmt->levels) {
        auto repr = Repr[level];
        buf->sputn(repr.data(), repr.size());
        level = Next[level];
    }
    fmt->ostr << tfmt::Reset;
}
