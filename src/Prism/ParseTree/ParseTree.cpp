#include "Prism/ParseTree/ParseTree.h"

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"

using namespace prism;

void* ParseTreeContext::allocate() { return allocate(0); }

void* ParseTreeContext::allocate(size_t numChildren) {
    return alloc.allocate(sizeof(Facet) + numChildren * sizeof(void*),
                          alignof(Facet));
}

using namespace tfmt::modifiers;

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

namespace {

struct PTPrinter {
    std::ostream& str;
    TreeFormatter& fmt;

    PTPrinter(std::ostream& str, TreeFormatter& fmt): str(str), fmt(fmt) {}

    void print(Facet const* node) {
        if (!node) {
            str << NullNode << "\n";
            return;
        }
        if (auto* term = csp::dyncast<TerminalFacet const*>(node)) {
            str << term->token().kind << "\n";
        }
        else {
            str << get_rtti(*node) << "\n";
        }
        fmt.writeChildren(node->children(),
                          [&](Facet const* child) { print(child); });
    }
};

} // namespace

void prism::print(Facet const* root, std::ostream& str) {
    TreeFormatter fmt(str);
    print(root, str, fmt);
}

void prism::print(Facet const* root, std::ostream& str, TreeFormatter& fmt) {
    PTPrinter(str, fmt).print(root);
}
