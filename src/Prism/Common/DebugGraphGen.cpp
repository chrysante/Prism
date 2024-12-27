#include "Prism/Common/DebugGraphGen.h"

#include <cstdlib>
#include <fstream>
#include <iostream>

#include <utl/strcat.hpp>

using namespace prism;

void prism::createDebugGraph(DebugGraphDescriptor const& desc) {
    if (!desc.generator) {
        std::cerr << "Missing generator\n";
        return;
    }
    auto dotFilePath = desc.targetDir / (desc.name + ".dot");
    std::fstream dotFile(dotFilePath, std::ios::out | std::ios::trunc);
    if (!dotFile) {
        std::cerr << "Failed to create file: " << dotFilePath << std::endl;
        return;
    }
    desc.generator(dotFile);
    dotFile.close();
    if (!desc.generateGraph) return;
    auto svgFilePath = desc.targetDir / (desc.name + ".svg");
    auto dotCommand =
        utl::strcat("dot -Tsvg -o ", svgFilePath, " ", dotFilePath);
    std::system(dotCommand.c_str());
    if (!desc.openFile) return;
    auto openCommand = utl::strcat("open ", svgFilePath);
    std::system(openCommand.c_str());
}
