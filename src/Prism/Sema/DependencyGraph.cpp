#include "Prism/Sema/DependencyGraph.h"

#include <ostream>
#include <queue>

#include <graphgen/generate.h>
#include <graphgen/graph.h>
#include <range/v3/algorithm.hpp>
#include <termfmt/termfmt.h>
#include <utl/strcat.hpp>

#include "Prism/Common/DebugGraphGen.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/SemaPrint.h"

using namespace prism;
using ranges::views::concat;
using ranges::views::single;
using ranges::views::transform;

DependencyGraph::TopsortResult DependencyGraph::doTopsort() const {
    utl::hashmap<DependencyNode const*, size_t> inDegrees;
    for (auto* node: nodes())
        for (auto* dep: node->dependencies())
            inDegrees[dep]++;
    // Collect nodes with in-degree 0
    std::queue<DependencyNode const*> zeroInDegree;
    for (auto* node: nodes())
        if (!inDegrees.contains(node)) zeroInDegree.push(node);
    // Perform topological sort
    utl::small_vector<Symbol*> topoOrder;
    size_t processedCount = 0;
    while (!zeroInDegree.empty()) {
        auto* node = zeroInDegree.front();
        zeroInDegree.pop();
        topoOrder.push_back(&node->symbol());
        processedCount++;
        for (auto* dep: node->dependencies())
            if (--inDegrees[dep] == 0) zeroInDegree.push(dep);
    }
    // Check for cycles
    if (processedCount == nodes().size())
        return { /* isCycle: */ false, std::move(topoOrder) };
    // There's a cycle. Extract it.
    utl::hashset<DependencyNode const*> visited, inStack;
    utl::small_vector<DependencyNode const*> stack;
    auto dfs = [&](auto& dfs,
                   DependencyNode const* node) -> DependencyNode const* {
        if (visited.contains(node)) return nullptr;
        visited.insert(node);
        inStack.insert(node);
        stack.push_back(node);
        for (auto* dep: node->dependencies()) {
            if (!visited.contains(dep)) {
                if (auto* cycleStart = dfs(dfs, dep)) return cycleStart;
            }
            else if (inStack.contains(dep)) {
                return dep; // Cycle start detected
            }
        }
        inStack.erase(node);
        stack.pop_back();
        return nullptr;
    };
    auto* cycleStart = [&] {
        for (auto* node: nodes()) {
            if (visited.contains(node)) continue;
            auto* cycleStart = dfs(dfs, node);
            if (cycleStart) return cycleStart;
        }
        PRISM_UNREACHABLE();
    }();
    // Extract the cycle from the stack
    auto cycleBegin = ranges::find(stack, cycleStart);
    auto cycle = concat(ranges::make_subrange(cycleBegin, stack.end()) |
                            transform(FN1(&_1->symbol())),
                        single(&cycleStart->symbol())) |
                 ToSmallVector<>;
    return { /* isCycle: */ true, std::move(cycle) };
}

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
