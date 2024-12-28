#include "Prism/Common/DebugGraphGen.h"

#include <cstdlib>
#include <fstream>
#include <iostream>

#include <utl/strcat.hpp>

using namespace prism;

static std::string_view toExtension(DebugGraphDescriptor::OutputType type) {
    using enum DebugGraphDescriptor::OutputType;
    switch (type) {
    case SVG:
        return ".svg";
    case PNG:
        return ".png";
    }
}

static std::string_view toGraphvizCmd(DebugGraphDescriptor::OutputType type) {
    using enum DebugGraphDescriptor::OutputType;
    switch (type) {
    case SVG:
        return "-Tsvg";
    case PNG:
        return "-Tpng";
    }
}

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
    auto svgFilePath =
        desc.targetDir / utl::strcat(desc.name, toExtension(desc.outputType));
    auto dotCommand = utl::strcat("dot ", toGraphvizCmd(desc.outputType),
                                  " -o ", svgFilePath, " ", dotFilePath);
    std::system(dotCommand.c_str());
    if (!desc.openFile) return;
    auto openCommand = utl::strcat("open ", svgFilePath);
    std::system(openCommand.c_str());
}
