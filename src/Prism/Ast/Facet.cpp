#include "Prism/Ast/Facet.h"

#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Ast/Ast.h"
#include "Prism/Ast/AstDump.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using namespace tfmt::modifiers;

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL FACET");
};

static constexpr utl::streammanip FacetName = [](std::ostream& str,
                                                 Facet const& facet) {
    tfmt::FormatGuard fmt(BrightBlue | Italic, str);
    if (auto* term = dyncast<TerminalFacet const*>(&facet)) {
        str << term->token().kind;
    }
    else {
        str << get_rtti(facet);
    }
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
        if (auto* wrapper = dyncast<AstWrapperFacet const*>(node)) {
            dumpAst(wrapper->get(), fmt);
            return;
        }
        str << FacetName(*node);
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
    print(root, fmt);
}

void prism::print(Facet const* root, TreeFormatter& fmt,
                  SourceContext const* srcCtx) {
    FacetPrinter{ fmt.ostream(), fmt, srcCtx }.print(root);
}
