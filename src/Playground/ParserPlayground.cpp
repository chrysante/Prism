#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>

#include <CLI/CLI.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include <Prism/Common/TreeFormatter.h>
#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Diagnostic/DiagnosticFormat.h>
#include <Prism/Facet/Facet.h>
#include <Prism/Lexer/Lexer.h>
#include <Prism/Parser/Parser.h>
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
    auto DE = makeDefaultDiagnosticEmitter();
    Lexer L(ctx, *DE);
    while (true) {
        auto tok = L.next();
        if (tok.kind == TokenKind::End) {
            return;
        }
        std::cout << tok.kind << ": " << ctx.getTokenStr(tok) << std::endl;
    }
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
    auto DE = makeDefaultDiagnosticEmitter();
    auto* tree = [&]() -> Facet const* {
        switch (options.mode) {
        case Mode::Program:
            return parseSourceFile(alloc, sourceContext, *DE);
        case Mode::Expr:
            return parseExpr(alloc, sourceContext, *DE);
        case Mode::Type:
            return parseTypeSpec(alloc, sourceContext, *DE);
        }
    }();
    TreeFormatter fmt(std::cout, { .lines = TreeStyle::Rounded });
    print(tree, fmt, { &sourceContext });
    if (!DE->empty()) {
        print(*DE);
        return 1;
    }
    return 0;
}
