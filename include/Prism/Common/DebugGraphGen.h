#ifndef PRISM_COMMON_DEBUGGRAPHGEN_H
#define PRISM_COMMON_DEBUGGRAPHGEN_H

#include <filesystem>
#include <functional>
#include <iosfwd>
#include <string>

namespace prism {

/// Options structure for `createDebugGraph()`
struct DebugGraphDescriptor {
    enum OutputType { SVG, PNG };

    /// Directory where to generate the files
    std::filesystem::path targetDir;

    /// Base name of the generated files
    std::string name = "Debug";

    /// Graphviz code generator
    std::function<void(std::ostream&)> generator;

    /// Output format to use
    OutputType outputType = OutputType::SVG;

    /// If true, executes `dot` command to generate the graph from the graphviz
    /// code
    bool generateGraph = true;

    /// If true, executes `open` command to open the generated file
    bool openFile = true;
};

/// Creates a visualized graph in a temporary file
void createDebugGraph(DebugGraphDescriptor const& desc);

} // namespace prism

#endif // PRISM_COMMON_DEBUGGRAPHGEN_H
