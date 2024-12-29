#include "Prism/Common/TreeFormatter.h"

#include <termfmt/termfmt.h>

using namespace prism;
using enum TreeFormatter::Level;

static constexpr TreeFormatter::Level Next[] = { ChildContinue, ChildContinue,
                                                 LastChildContinue,
                                                 LastChildContinue };

static constexpr std::array<std::string_view, 4> Repr[] = {
    { "  ", "  ", "  ", "  " },
    { "+-", "| ", "+-", "  " },
    { "├╴", "│ ", "└╴", "  " },
    { "├╴", "│ ", "╰╴", "  " },
};

static void write(std::streambuf* buf, std::string_view text) {
    buf->sputn(text.data(), (std::streamsize)text.size());
}

void TreeFormatter::Indenter::operator()(std::streambuf* buf) const {
    static auto const Mod = tfmt::BrightGrey | tfmt::Bold;
    bool useAnsiCodes = tfmt::isTermFormattable(fmt->ostr);
    if (useAnsiCodes) {
        write(buf, tfmt::Reset.ansiBuffer());
        write(buf, Mod.ansiBuffer());
    }
    auto const& repr = Repr[fmt->style.lines];
    for (auto& level: fmt->levels) {
        auto reprStr = repr[level];
        write(buf, reprStr);
        level = Next[level];
    }
    if (useAnsiCodes) {
        write(buf, tfmt::Reset.ansiBuffer());
        tfmt::reapplyModifiers(fmt->ostr);
    }
}
