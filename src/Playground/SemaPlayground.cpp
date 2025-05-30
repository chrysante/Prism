#include <functional>
#include <iostream>
#include <map>
#include <memory>

#include <CLI/CLI.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include <Prism/Common/TreeFormatter.h>
#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Diagnostic/DiagnosticFormat.h>
#include <Prism/Invocation/Invocation.h>
#include <Prism/Sema/SemaPrint.h>
#include <Prism/Sema/Symbol.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;

CLI::App* addSubcommand(std::string name, std::function<int()>);

namespace {

struct Options {
    bool printFacets = false;
    bool printConformances = false;
    bool printScopes = false;
};

} // namespace

static int semaPlaygroundMain(Options);

static int const INIT = [] {
    auto options = std::make_shared<Options>();
    auto* cmd =
        addSubcommand("sema", [=] { return semaPlaygroundMain(*options); });
    cmd->add_flag("--print-facets", options->printFacets);
    cmd->add_flag("--print-conformances", options->printConformances);
    cmd->add_flag("--print-scopes", options->printScopes);
    return 0;
}();

static void header(std::ostream& str, std::string_view title) {
    utl::streammanip repeat = [](std::ostream& str, int n, std::string_view c) {
        for (int i = 0; i < n; ++i)
            str << c;
    };
    size_t numCols = tfmt::getWidth(str).value_or(80);
    size_t innerWidth = (size_t)std::max((ssize_t)numCols - 2, ssize_t{});
    title =
        title.substr(0, (size_t)std::max((ssize_t)innerWidth - 2, ssize_t{}));
    size_t remainingSpace = innerWidth - title.size();
    size_t leftSpace = remainingSpace / 7;
    size_t rightSpace = remainingSpace - leftSpace;
    using namespace tfmt::modifiers;
    auto mod = BrightGrey | Bold;
    str << tfmt::format(mod, "=", repeat(numCols - 2, "="), "=") << "\n";
    str << tfmt::format(mod, "=", repeat(leftSpace, " "),
                        tfmt::format(Reset | Bold, title),
                        repeat(rightSpace, " "), "=")
        << "\n";
    str << tfmt::format(mod, "=", repeat(numCols - 2, "="), "=") << "\n";
}

static int semaPlaygroundMain(Options options) {
    std::filesystem::path filepath = "examples/Playground.prism";
    std::fstream file(filepath);
    Invocation inv;
    inv.add_source_file(filepath);
    inv.run_until(InvocationStage::Sema);
    if (options.printFacets) {
        header(std::cout, "Parse Tree");
        std::cerr << tfmt::format(tfmt::Red | tfmt::Bold,
                                  "Facet printing not implemented")
                  << "\n";
#if 0
        TreeFormatter fmt(std::cout, { .lines = TreeStyle::Rounded });
        auto* parseTree = inv.get_parse_tree(filepath);
        print(parseTree, fmt, { &sourceContext });
#endif
    }
    auto* mod = inv.get_module();
    if (mod) {
        header(std::cout, "Sema IR");
        print(*mod, std::cout);
    }
    auto& DE = inv.get_diagnostic_emitter();
    if (!DE.empty()) {
        print(DE);
        return 1;
    }
    return 0;
}
