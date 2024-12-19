#include "Prism/Facet/Facet.h"

#include <string_view>
#include <vector>

#include <termfmt/termfmt.h>
#include <utl/hashtable.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using namespace tfmt::modifiers;

static constexpr utl::streammanip Secondary = [](std::ostream& str,
                                                 auto const&... args) {
    str << tfmt::format(BrightGrey, args...);
};

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

static constexpr utl::streammanip FacetName = [](std::ostream& str,
                                                 Facet const& facet) {
    tfmt::FormatGuard fmt(Green | Italic, str);
    if (auto* term = dyncast<TerminalFacet const*>(&facet)) {
        str << term->token().kind;
    }
    else {
        str << get_rtti(facet);
    }
};

static utl::hashmap<FacetType, std::vector<std::string_view>> const
    ChildNameMap = {
#include "Prism/Facet/FacetMemberNames.inl"
    };

static std::string_view childName(Facet const& node, size_t index) {
    auto itr = ChildNameMap.find(get_rtti(node));
    PRISM_ASSERT(itr != ChildNameMap.end());
    auto& elems = itr->second;
    if (index < elems.size()) return elems[index];
    return {};
}

namespace {

struct FacetPrinter: FacetPrintOptions {
    std::ostream& str;
    TreeFormatter& fmt;

    void print(Facet const* node, Facet const* parent, size_t index) {
        if (!node) {
            str << NullNode;
            invokeCallback(node, parent, index);
            str << "\n";
            return;
        }
        str << FacetName(*node);
        csp::visit(*node, [this](auto& node) { details(node); });
        invokeCallback(node, parent, index);
        str << "\n";
        writeChildren(*node);
    }

    void writeChildren(Facet const& node) {
        fmt.writeChildren(node.children(),
                          [&](size_t index, Facet const* child) {
            auto name = childName(node, index);
            if (!name.empty()) str << Secondary(name, ": ");
            print(child, &node, index);
        });
    }

    void details(Facet const&) {}

    void details(TerminalFacet const& term) {
        if (srcCtx) {
            str << " " << srcCtx->getTokenStr(term.token());
        }
    }

    void invokeCallback(Facet const* facet, Facet const* parent, size_t index) {
        if (nodeCallback) nodeCallback(str, facet, parent, index);
    }
};

} // namespace

void prism::print(Facet const* root, std::ostream& str,
                  FacetPrintOptions options) {
    TreeFormatter fmt(str);
    print(root, fmt, options);
}

void prism::print(Facet const* root, TreeFormatter& fmt,
                  FacetPrintOptions options) {
    FacetPrinter{ options, fmt.ostream(), fmt }.print(root, nullptr, 0);
}
