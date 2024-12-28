#include "Prism/Common/Issue.h"

#include <iomanip>
#include <ostream>

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using namespace tfmt::modifiers;

void Issue::format(std::ostream& str, SourceContext const& ctx) const {
    TreeFormatter fmt(str);
    formatImpl(fmt, ctx);
}

static auto fmt(Issue::Kind kind) {
    return utl::streammanip([=](std::ostream& str) {
        using enum Issue::Kind;
        switch (kind) {
        case Error:
            str << tfmt::format(Bold | BrightRed, "Error:") << " ";
            break;
        case Warning:
            str << tfmt::format(Bold | BrightYellow, "Warning:") << " ";
            break;
        case Note:
            break;
        }
    });
}

static auto fmt(SourceLocation loc) {
    return utl::streammanip([=](std::ostream& str) {
        str << "L:" << tfmt::format(Bold, loc.line + 1)
            << " C:" << tfmt::format(Bold, loc.column + 1);
    });
}

static SourceRange expandToWholeLines(SourceContext const& ctx,
                                      SourceRange range) {
    std::string_view source = ctx.source();
    uint32_t begin = range.index;
    while (begin > 0 && source[begin - 1] != '\n')
        --begin;
    uint32_t end = range.index + range.length;
    while (end < source.size() && source[end] != '\n')
        ++end;
    if (end < source.size()) ++end;
    return { begin, end - begin };
}

static void forEachLine(std::string_view text,
                        std::invocable<std::string_view> auto f) {
    while (!text.empty()) {
        size_t index = text.find('\n');
        if (index == std::string_view::npos) {
            f(text);
            return;
        }
        f(text.substr(0, index));
        text = text.substr(index + 1);
    }
}

static void printSourceRange(SourceContext const& ctx, SourceRange range,
                             tfmt::Modifier const& background,
                             tfmt::Modifier const& highlight,
                             std::ostream& str) {
    SourceRange wholeRange = expandToWholeLines(ctx, range);
    std::string_view snipped = ctx.source(wholeRange);
    size_t lineIndex = ctx.getSourceLocation(wholeRange.index).line;
    forEachLine(snipped, [&, first = true](std::string_view line) mutable {
        if (!first) str << "\n";
        first = false;
        str << tfmt::format(BrightGrey, std::setw(5), ++lineIndex, " | ");
        str << tfmt::format(BrightGrey, line);
    });
}

static tfmt::Modifier getHighlightMod(Issue::Kind kind) {
    using enum Issue::Kind;
    switch (kind) {
    case Error:
        return BrightWhite | BGBrightRed;
    case Warning:
        return BrightWhite | BGYellow;
    case Note:
        return BrightWhite | BGBrightBlue;
    }
}

void Issue::formatImpl(TreeFormatter& treeFmt, SourceContext const& ctx) const {
    auto& str = treeFmt.ostream();
    str << fmt(kind());
    auto range = sourceRange();
    if (range) str << fmt(ctx.getSourceLocation(range->index)) << " ";
    header(str, ctx);
    str << "\n";
    if (range) {
        treeFmt.writeDetails(children().empty(), [&] {
            printSourceRange(ctx, *range, BrightGrey, getHighlightMod(kind()),
                             str);
        });
    }
    treeFmt.writeChildren(children(), [&](Issue const* child) {
        child->formatImpl(treeFmt, ctx);
    });
}
