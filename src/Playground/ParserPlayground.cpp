#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>

#include <CLI/CLI.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include <Prism/Common/IssueHandler.h>
#include <Prism/Common/TreeFormatter.h>
#include <Prism/Facet/Facet.h>
#include <Prism/Lexer/Lexer.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Sema/Construction.h>
#include <Prism/Sema/SemaContext.h>
#include <Prism/Sema/Symbol.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;

CLI::App* addSubcommand(std::string name, std::function<int()>);

namespace {

enum class Mode { Program, Expr, Type };

struct Options {
    Mode mode{};
};

} // namespace

static int parserPlaygroundMain(Options);

static int const INIT = [] {
    auto options = std::make_shared<Options>();
    auto* cmd =
        addSubcommand("parser", [=] { return parserPlaygroundMain(*options); });
    std::map<std::string, Mode> modeMap = { { "file", Mode::Program },
                                            { "expr", Mode::Expr },
                                            { "type", Mode::Type } };
    cmd->add_option("-m,--mode", options->mode, "Parsing mode")
        ->transform(
            CLI::CheckedTransformer(std::move(modeMap), CLI::ignore_case));
    return 0;
}();

[[maybe_unused]]
static void printTokenStream(std::string_view source) {
    SourceContext ctx({}, source);
    IssueHandler issueHandler;
    Lexer L(source, issueHandler);
    while (true) {
        auto tok = L.next();
        if (tok.kind == TokenKind::End) {
            return;
        }
        std::cout << tok.kind << ": " << ctx.getTokenStr(tok) << std::endl;
    }
}

static void header(std::ostream& str, std::string_view title) {
    utl::streammanip repeat = [](std::ostream& str, int n, std::string_view c) {
        for (int i = 0; i < n; ++i)
            str << c;
    };
    size_t numCols = tfmt::getWidth(str).value_or(80);
    size_t innerWidth = std::max(numCols - 2, size_t{});
    title = title.substr(0, std::max(innerWidth - 2, size_t{}));
    size_t remainingSpace = innerWidth - title.size();
    size_t leftSpace = remainingSpace / 7;
    size_t rightSpace = remainingSpace - leftSpace;
    using namespace tfmt::modifiers;
    auto mod = BrightMagenta | Bold;
    str << tfmt::format(mod, "╦", repeat(numCols - 2, "═"), "╦") << "\n";
    str << tfmt::format(mod, "║", repeat(leftSpace, " "),
                        tfmt::format(Reset | Bold, title),
                        repeat(rightSpace, " "), "║")
        << "\n";
    str << tfmt::format(mod, "╩", repeat(numCols - 2, "═"), "╩") << "\n";
}

static int parserPlaygroundMain(Options options) {

    std::filesystem::path filepath = "examples/Playground.prism";
    std::fstream file(filepath);
    if (!file) {
        std::cerr << "Failed to open file\n";
        return 1;
    }
    std::stringstream sstr;
    sstr << file.rdbuf();
    std::string source = std::move(sstr).str();
    MonotonicBufferResource alloc;
    SourceContext sourceContext(filepath, source);
    IssueHandler issueHandler;
    auto* tree = [&]() -> Facet const* {
        switch (options.mode) {
        case Mode::Program:
            return parseSourceFile(alloc, sourceContext, issueHandler);
        case Mode::Expr:
            return parseExpr(alloc, sourceContext, issueHandler);
        case Mode::Type:
            return parseTypeSpec(alloc, sourceContext, issueHandler);
        }
    }();
    TreeFormatter fmt(std::cout, { .lines = TreeStyle::Rounded });
    header(std::cout, "Parse Tree");
    print(tree, fmt, { &sourceContext });
    if (!issueHandler.empty()) {
        issueHandler.print(sourceContext);
        return 1;
    }
    return 0;
}
