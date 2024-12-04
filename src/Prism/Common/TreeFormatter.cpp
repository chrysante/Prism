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

void TreeFormatter::Indenter::operator()(std::streambuf* buf) const {
    static auto const Mod = tfmt::BrightGrey | tfmt::Bold;
    buf->sputn(Mod.ansiBuffer().data(), Mod.ansiBuffer().size());
    auto const& repr = Repr[fmt->style.lines];
    for (auto& level: fmt->levels) {
        auto reprStr = repr[level];
        buf->sputn(reprStr.data(), reprStr.size());
        level = Next[level];
    }
    buf->sputn(tfmt::Reset.ansiBuffer().data(),
               tfmt::Reset.ansiBuffer().size());
}
