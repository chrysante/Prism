#include "Prism/Facet/Facet.h"

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using namespace tfmt::modifiers;

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

namespace {

struct FacetPrinter {
    std::ostream& str;
    TreeFormatter& fmt;
    SourceContext const* srcCtx;

    void print(Facet const* node) {
        if (!node) {
            str << NullNode << "\n";
            return;
        }
        if (auto* term = csp::dyncast<TerminalFacet const*>(node)) {
            str << term->token().kind;
        }
        else {
            str << get_rtti(*node);
        }
        csp::visit(*node, [this](auto& node) { details(node); });
        str << "\n";
        fmt.writeChildren(node->children(),
                          [&](Facet const* child) { print(child); });
    }

    void details(Facet const&) {}

    void details(TerminalFacet const& term) {
        if (srcCtx) {
            str << " " << srcCtx->getTokenStr(term.token());
        }
    }
};

} // namespace

void prism::print(Facet const* root, std::ostream& str,
                  SourceContext const* srcCtx) {
    TreeFormatter fmt(str);
    print(root, str, fmt);
}

void prism::print(Facet const* root, std::ostream& str, TreeFormatter& fmt,
                  SourceContext const* srcCtx) {
    FacetPrinter{ str, fmt, srcCtx }.print(root);
}
