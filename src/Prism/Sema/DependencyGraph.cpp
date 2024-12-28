#include "Prism/Sema/DependencyGraph.h"

#include <ostream>

#include <graphgen/generate.h>
#include <graphgen/graph.h>
#include <termfmt/termfmt.h>
#include <utl/strcat.hpp>

#include "Prism/Common/DebugGraphGen.h"
#include "Prism/Sema/SemaPrint.h"

using namespace prism;

void prism::generateGraphviz(DependencyGraph const& graph, std::ostream& str) {
    using namespace graphgen;
    Graph G;
    G.font("monospace");
    tfmt::setHTMLFormattable(str);
    for (auto* node: graph) {
        auto* vertex = Vertex::make(ID(node));
        vertex->label(formatDecl(node->symbol()), LabelKind::HTML);
        G.add(vertex);
        for (auto* dependency: node->dependencies())
            G.add(Edge{ ID(node), ID(dependency) });
    }
    generate(G, str);
}

void prism::generateGraphvizDebug(DependencyGraph const& graph) {
    createDebugGraph(
        { .name = "DependencyGraph",
          .targetDir = "build",
          .generator = [&](std::ostream& str) { generateGraphviz(graph, str); },
          .outputType = DebugGraphDescriptor::PNG });
}
