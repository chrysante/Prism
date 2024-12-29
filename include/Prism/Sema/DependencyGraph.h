#ifndef PRISM_SEMA_DEPENDENCYGRAPH_H
#define PRISM_SEMA_DEPENDENCYGRAPH_H

#include <iosfwd>
#include <span>

#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Allocator.h>
#include <Prism/Common/Assert.h>
#include <Prism/Common/Functional.h>

namespace prism {

class Symbol;

/// Node in the dependency graph. Specifies which symbols depend on which other
/// symbols
class DependencyNode {
public:
    explicit DependencyNode(Symbol& symbol, MonotonicBufferResource& resource):
        sym(symbol), succs(&resource) {}

    /// Tracks \p node  as a dependency
    void addDependency(DependencyNode const* node) {
        if (node) succs.insert(node);
    }

    /// \Returns the associated symbol
    Symbol& symbol() const { return sym; }

    /// \Returns a list of the dependencies of this node
    std::span<DependencyNode const* const> dependencies() const {
        return succs.values();
    }

private:
    using AllocType =
        ResourceAllocator<DependencyNode const*, MonotonicBufferResource>;
    utl::hashset<DependencyNode const*, utl::hash<DependencyNode const*>,
                 std::equal_to<>, AllocType>
        succs;
    Symbol& sym;
};

/// Graph tracking dependencies between symbols. This is used to track
/// instantiation dependencies, e.g., to detect cyclic structs
///
///     struct S { var t: T; }
///     struct T { var s: S; }
///
/// But the design is more general and can be used to track all kinds of
/// dependencies between symbols.
class DependencyGraph {
    auto nodes() const {
        return map | ranges::views::values |
               ranges::views::transform(ToConstAddress);
    }

public:
    explicit DependencyGraph(MonotonicBufferResource& resource):
        map(&resource) {}

    /// \Returns the node associated with the symbol \p sym
    DependencyNode* getNode(Symbol& sym) {
        auto itr = map.find(&sym);
        if (itr != map.end()) return itr->second;
        auto* resource = getResource();
        auto* node = allocate<DependencyNode>(*resource, sym, *resource);
        map.insert({ &sym, node });
        return node;
    }

    /// Iterator interface @{
    auto begin() const { return nodes().begin(); }
    auto end() const { return nodes().end(); }
    /// @}

    /// Topologically sorts the graph
    void topsort() { tsResult = doTopsort(); }

    /// \Returns true if this graph has a cycle.
    /// \pre `topsort()` must be called before
    bool hasCycle() const { return tsResult.value().isCycle; }

    /// \Returns the cycle in this graph
    /// \pre `topsort()` must be called before and `hasCycle()` must be true
    std::span<Symbol* const> getCycle() const {
        PRISM_ASSERT(hasCycle());
        return tsResult.value().symbols;
    }

    /// \Returns the topological ordering of this graph
    /// \pre `topsort()` must be called before and `hasCycle()` must be false
    std::span<Symbol* const> getTopoOrder() const {
        PRISM_ASSERT(!hasCycle());
        return tsResult.value().symbols;
    }

private:
    MonotonicBufferResource* getResource() const {
        return map.get_allocator().resource();
    }

    /// Result structure for `doTopsort()`
    struct TopsortResult {
        bool isCycle;
        utl::small_vector<Symbol*> symbols;
    };

    /// \Returns a topological order of the graph or a cycle if the graph is not
    /// acyclic
    TopsortResult doTopsort() const;

    using AllocType =
        ResourceAllocator<std::pair<Symbol const*, DependencyNode*>,
                          MonotonicBufferResource>;

    utl::hashmap<Symbol const*, DependencyNode*, utl::hash<Symbol const*>,
                 std::equal_to<>, AllocType>
        map;

    std::optional<TopsortResult> tsResult;
};

/// Writes \p graph into \p ostream as graphviz code
void generateGraphviz(DependencyGraph const& graph, std::ostream& ostream);

/// Generates and opens a visual representation of \p graph
void generateGraphvizDebug(DependencyGraph const& graph);

} // namespace prism

#endif // PRISM_SEMA_DEPENDENCYGRAPH_H
