#include "Prism/Diagnostic/Diagnostic.h"

#include <iomanip>
#include <ostream>

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using namespace tfmt::modifiers;

void Diagnostic::format(std::ostream& str, SourceContext const*) const {
    TreeFormatter fmt(str);
    formatImpl(fmt, sourceContext());
}

std::optional<FullSourceRange> Diagnostic::sourceRange() const {
    auto* ctx = sourceContext();
    if (!ctx) return std::nullopt;
    return ctx->getFullSourceRange(_sourceRange);
}

Diagnostic::Diagnostic(Kind kind, std::optional<SourceRange> sourceRange,
                       SourceContext const* context):
    _kind(kind),
    _sourceRange(sourceRange.value_or(SourceRange{})),
    _sourceContext(context) {
    PRISM_ASSERT((bool)context == sourceRange.has_value(),
                 "We must have a source range iff. we have a context");
}

static auto fmt(Diagnostic::Kind kind) {
    return utl::streammanip([=](std::ostream& str) {
        using enum Diagnostic::Kind;
        switch (kind) {
        case Error:
            str << tfmt::format(Bold | BrightRed, "Error:") << " ";
            break;
        case Warning:
            str << tfmt::format(Bold | BrightYellow, "Warning:") << " ";
            break;
        case Note:
            break;
        case Hint:
            str << tfmt::format(Bold | BrightGreen, "Hint:") << " ";
            break;
        }
    });
}

static auto fmt(SourceContext const& ctx, SourceLocation loc) {
    return utl::streammanip([=](std::ostream& str) {
        tfmt::FormatGuard guard(BrightGrey, str);
        str << ctx.filepath().filename()
            << " L:" << tfmt::format(Bold, loc.line + 1)
            << " C:" << tfmt::format(Bold, loc.column + 1);
    });
}

static SourceRange expandToWholeLines(SourceContext const& ctx,
                                      SourceRange range, int startPadding,
                                      int endPadding) {
    std::string_view source = ctx.source();
    uint32_t begin = range.index;
    do {
        while (begin > 0 && source[begin - 1] != '\n')
            --begin;
        if (begin > 0 && startPadding > 0) --begin;
    } while (startPadding-- > 0);
    uint32_t end = range.index + range.length;
    do {
        while (end < source.size() && source[end] != '\n')
            ++end;
        if (end < source.size() > 0 && endPadding > 0) ++end;
    } while (endPadding-- > 0);
    if (end < source.size()) ++end;
    return { begin, end - begin };
}

static void forEachLine(std::string_view text, SourceRange wholeRange,
                        SourceRange range,
                        std::invocable<std::string_view, std::string_view,
                                       std::string_view> auto f) {
    PRISM_ASSERT(wholeRange.index <= range.index);
    PRISM_ASSERT(wholeRange.index + wholeRange.length >=
                 range.index + range.length);
    range.index -= wholeRange.index;
    wholeRange.index = 0;
    while (!text.empty()) {
        size_t breakIndex = text.find('\n');
        auto line = text.substr(0, breakIndex);
        size_t highlightStart = std::min(breakIndex, size_t{ range.index });
        size_t highlightEnd =
            std::min(breakIndex, highlightStart + range.length);
        while (highlightStart < line.size() &&
               std::isspace(line[highlightStart]))
            ++highlightStart;
        while (highlightEnd > 0 && std::isspace(line[highlightEnd - 1]))
            --highlightEnd;
        PRISM_ASSERT(highlightStart <= highlightEnd);
        size_t highlightCount = highlightEnd - highlightStart;
        auto begin = line.substr(0, highlightStart);
        auto highlight = line.substr(highlightStart, highlightCount);
        auto end = line.substr(highlightEnd);
        f(begin, highlight, end);
        text = breakIndex == std::string_view::npos ?
                   std::string_view{} :
                   text.substr(breakIndex + 1);
        wholeRange.index = 0;
        if (line.size() + 1 > range.index) {
            range.length -=
                std::min(size_t{ range.length }, line.size() + 1 - range.index);
            range.index = 0;
        }
        else {
            range.index -= line.size() + 1;
        }
    }
}

static auto fill(size_t num, std::string_view first = " ",
                 std::string_view remaining = " ",
                 tfmt::Modifier const& mod = None) {
    return utl::streammanip([=](std::ostream& str) {
        tfmt::FormatGuard guard(mod, str);
        if (num > 0) str << first;
        for (size_t i = 1; i < num; ++i)
            str << remaining;
    });
}

static constexpr utl::streammanip StartSourceLine = [](std::ostream& str,
                                                       auto const& lineIndex) {
    str << tfmt::format(BrightGrey, std::setw(5), lineIndex, " | ");
};

static void printSourceRange(SourceContext const& ctx, SourceRange range,
                             std::ostream& str) {
    SourceRange wholeRange = expandToWholeLines(ctx, range, 1, 1);
    std::string_view snipped = ctx.source(wholeRange);
    size_t lineIndex = ctx.getSourceLocation(wholeRange.index).line;
    forEachLine(snipped, wholeRange, range,
                [&, first = true](std::string_view begin,
                                  std::string_view highlight,
                                  std::string_view end) mutable {
        if (!first) str << "\n";
        str << StartSourceLine(++lineIndex) << tfmt::format(BrightGrey, begin)
            << tfmt::format(Bold, highlight) << tfmt::format(BrightGrey, end);
        if (!highlight.empty()) {
            std::string_view startIndicator = "^";
            std::string_view indicator = "~";
            std::string_view start = first ? startIndicator : indicator;
            str << "\n"
                << StartSourceLine("") << fill(begin.size())
                << fill(highlight.size(), start, indicator, Bold | Green)
                << fill(end.size());
        }
        first = false;
    });
}

void Diagnostic::formatImpl(TreeFormatter& treeFmt,
                            SourceContext const* ctx) const {
    auto& str = treeFmt.ostream();
    str << fmt(kind());
    auto range = sourceRange();
    PRISM_ASSERT(!range || ctx);
    if (range) str << fmt(*ctx, range->begin) << " ";
    header(str, ctx);
    str << "\n";
    if (range) {
        treeFmt.writeDetails(children().empty(), [&] {
            printSourceRange(*ctx, range->slim(), str);
        });
    }
    treeFmt.writeChildren(children(), [&](Diagnostic const* child) {
        child->formatImpl(treeFmt, ctx);
    });
}
