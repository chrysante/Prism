#include "Prism/Common/Diagnostic.h"

#include <iomanip>
#include <ostream>

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using namespace tfmt::modifiers;

void Diagnostic::format(std::ostream& str, SourceContext const* ctx) const {
    TreeFormatter fmt(str);
    formatImpl(fmt, ctx);
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
                             std::ostream& str) {
    SourceRange wholeRange = expandToWholeLines(ctx, range);
    std::string_view snipped = ctx.source(wholeRange);
    size_t lineIndex = ctx.getSourceLocation(wholeRange.index).line;
    forEachLine(snipped, [&, first = true](std::string_view line) mutable {
        if (!first) str << "\n";
        first = false;
        str << tfmt::format(BrightGrey, std::setw(5), ++lineIndex, " ║ ");
        str << tfmt::format(BrightGrey, line);
    });
}

void Diagnostic::formatImpl(TreeFormatter& treeFmt,
                            SourceContext const* ctx) const {
    auto& str = treeFmt.ostream();
    str << fmt(kind());
    auto range = sourceRange();
    PRISM_ASSERT(!range || ctx);
    if (range) str << fmt(range->begin) << " ";
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
