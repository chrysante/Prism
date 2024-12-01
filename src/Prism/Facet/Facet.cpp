#include "Prism/Facet/Facet.h"

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"

using namespace prism;

using namespace tfmt::modifiers;

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

namespace {

struct FacetPrinter {
    std::ostream& str;
    TreeFormatter& fmt;

    FacetPrinter(std::ostream& str, TreeFormatter& fmt): str(str), fmt(fmt) {}

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
        csp::visit(*node, [this](auto& node) { details(node); });
        str << "\n";
        fmt.writeChildren(node->children(),
                          [&](Facet const* child) { print(child); });
    }

    void details(Facet const&) {}
};

} // namespace

void prism::print(Facet const* root, std::ostream& str) {
    TreeFormatter fmt(str);
    print(root, str, fmt);
}

void prism::print(Facet const* root, std::ostream& str, TreeFormatter& fmt) {
    FacetPrinter(str, fmt).print(root);
}
