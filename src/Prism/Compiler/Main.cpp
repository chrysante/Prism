#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

#include <CLI/CLI.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include <Prism/Common/Allocator.h>
#include <Prism/Common/TreeFormatter.h>
#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Diagnostic/DiagnosticFormat.h>
#include <Prism/Facet/Facet.h>
#include <Prism/Lexer/Lexer.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

namespace {

enum class TargetRepr { ParseTree };

struct CompilerOptions {
    std::vector<std::filesystem::path> inputFiles;
    std::optional<TargetRepr> targetRepr;
};

} // namespace

int compilerMain(CompilerOptions const& options);

int main(int argc, char* argv[]) {
    CLI::App app;
    CompilerOptions options;
    app.add_option("files", options.inputFiles, "input files");
    std::map<std::string, TargetRepr> targetReprMap = {
        { "parse-tree", TargetRepr::ParseTree }
    };
    app.add_option("--emit", options.targetRepr)
        ->transform(CLI::CheckedTransformer(std::move(targetReprMap)));
    try {
        app.parse(argc, argv);
        return compilerMain(options);
    }
    catch (CLI::ParseError const& e) {
        return app.exit(e);
    }
}

static constexpr utl::streammanip Error = [](std::ostream& str) {
    str << tfmt::format(tfmt::Red | tfmt::Bold, "Error: ");
};

int compilerMain(CompilerOptions const& options) {
    if (options.inputFiles.empty()) {
        std::cerr << Error << "no input files\n";
        return -1;
    }
    if (options.inputFiles.size() > 1) {
        std::cerr << Error << "only single file mode is supported for now\n";
        return -2;
    }
    if (options.targetRepr != TargetRepr::ParseTree) {
        std::cerr << Error << "must use option '--emit parse-tree' for now\n";
        return -3;
    }
    auto filepath = options.inputFiles.front();
    std::fstream file(filepath);
    if (!file) {
        std::cerr << Error << "failed to open file " << filepath << ": "
                  << std::strerror(errno) << "\n";
        return -4;
    }
    std::stringstream sstr;
    sstr << file.rdbuf();
    std::string source = std::move(sstr).str();
    prism::MonotonicBufferResource alloc;
    prism::SourceContext sourceContext(filepath, source);
    auto DE = prism::makeDefaultDiagnosticEmitter();
    auto* tree = prism::parseSourceFile(alloc, sourceContext, *DE);
    prism::TreeFormatter fmt(std::cout, { .lines = prism::TreeStyle::Rounded });
    prism::print(tree, fmt, { &sourceContext });
    if (!DE->empty()) print(*DE, std::cerr);
    if (DE->hasErrors()) return -5;
    return 0;
}
