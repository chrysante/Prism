#ifndef PRISM_COMMON_DEBUGGRAPHGEN_H
#define PRISM_COMMON_DEBUGGRAPHGEN_H

#include <filesystem>
#include <functional>
#include <iosfwd>

namespace prism {

struct DebugGraphDescriptor {
    std::filesystem::path targetDir;
    std::string name = "Debug";
    std::function<void(std::ostream&)> generator;
    bool generateGraph = true;
    bool openFile = true;
};

void createDebugGraph(DebugGraphDescriptor const& desc);

} // namespace prism

#endif // PRISM_COMMON_DEBUGGRAPHGEN_H
