#include <iostream>
#include <random>
#include <vector>

#include <CLI/CLI.hpp>
#include <range/v3/view.hpp>

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

static std::vector<char> makeRandomBits(uint64_t seed, size_t count) {
    std::mt19937_64 rng(seed);
    std::uniform_int_distribution<uint32_t> dist(0, 255);
    return ranges::views::generate([&] { return (char)dist(rng); }) |
           ranges::views::take(count) | ranges::to<std::vector>;
}

CLI::App* addSubcommand(std::string name, std::function<int()>);

static int fuzzParserMain();

static int const INIT = [] {
    addSubcommand("fuzz-parser", fuzzParserMain);
    return 0;
}();

void printIndex(int index) {
    // Clear the line and move the cursor to the start
    std::cout << "\r\033[K" << "Index: " << index << std::flush;
}
static int fuzzParserMain() {
    for (int i = 0;; ++i) {
        uint64_t seed = std::random_device{}();
        auto bytes = makeRandomBits(seed, 1 << 14);
        prism::MonotonicBufferResource alloc;
        prism::SourceContext ctx("invalid-path",
                                 { bytes.data(), bytes.size() });
        auto DE = prism::makeDefaultDiagnosticEmitter();
        prism::parseSourceFile(alloc, ctx, *DE);
        printIndex(i);
    }
}
