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
#include <Prism/Parser/Parser.h>
#include <Prism/Sema/Construction.h>
#include <Prism/Sema/SemaContext.h>
#include <Prism/Sema/Symbol.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;

CLI::App* addSubcommand(std::string name, std::function<int()>);

namespace {

struct Options {
    bool printFacets = false;
};

} // namespace

static int semaPlaygroundMain(Options);

static int const INIT = [] {
    auto options = std::make_shared<Options>();
    auto* cmd =
        addSubcommand("sema", [=] { return semaPlaygroundMain(*options); });
    cmd->add_flag("--facets", options->printFacets);
    return 0;
}();

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

static int semaPlaygroundMain(Options options) {
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
    auto* parseTree = parseSourceFile(alloc, sourceContext, issueHandler);
    if (options.printFacets) {
        TreeFormatter fmt(std::cout, { .lines = TreeStyle::Rounded });
        header(std::cout, "Parse Tree");
        print(parseTree, fmt, { &sourceContext });
    }
    if (!issueHandler.empty()) {
        issueHandler.print(sourceContext);
        return 1;
    }
    SemaContext ctx;
    auto* target = constructTarget(ctx, { { { parseTree, sourceContext } } });
    header(std::cout, "Sema IR");
    print(*target, std::cout);
    return 0;
}
