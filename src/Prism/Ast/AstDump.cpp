#include "Prism/Ast/AstDump.h"

#include <concepts>

#include <termfmt/termfmt.h>
#include <utl/hashtable.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Ast/Ast.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using namespace tfmt::modifiers;

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

static constexpr utl::streammanip NodeType = [](std::ostream& str,
                                                AstNode const& node) {
    str << tfmt::format(Green | Italic,
                        to_string_view(get_rtti(node)).substr(3));
};

static constexpr utl::streammanip Secondary = [](std::ostream& str,
                                                 auto const&... args) {
    str << tfmt::format(BrightGrey, args...);
};

static constexpr utl::streammanip Op = [](std::ostream& str,
                                          auto const&... args) {
    str << tfmt::format(Bold, args...);
};

static constexpr utl::streammanip Path = [](std::ostream& str,
                                            std::filesystem::path const& path) {
    if (path.empty()) {
        str << Secondary("<no filepath>");
    }
    else {
        str << Secondary('<', path, '>');
    }
};

template <typename K, typename V>
static V const& get(utl::hashmap<K, V> const& m, K const& key, V const& def) {
    auto itr = m.find(key);
    return itr != m.end() ? itr->second : def;
}

namespace {

struct Emitter {
    SourceContext const* ctx = nullptr;
    std::ostream& str;
    TreeFormatter& fmt;
    utl::hashmap<AstNodeType, utl::hashmap<size_t, std::string>> labelMap;

    Emitter(std::ostream& str, TreeFormatter& fmt):
        str(str),
        fmt(fmt),
        // clang-format off
        labelMap({
            { AstNodeType::AstParamDecl, { { 0, "Name" }, { 1, "Type" } } },
            { AstNodeType::AstFuncDecl, { { 0, "Name" }, { 2, "RetType" }, { 3, "Body" } } },
            { AstNodeType::AstArithmeticExpr, { { 0, "LHS" }, { 1, "RHS" } } },
        }) {} // clang-format on

    void run(AstNode const* root) { dfs(root); }

    void dfs(AstNode const* node) {
        if (!node) {
            str << NullNode << "\n";
            return;
        }
        csp::visit(*node, [&](auto& node) {
            str << NodeType(node) << " ";
            label(node);
            str << "\n";
            writeChildren(node);
            endNode(node);
        });
    }

    void label(AstNode const& node) {}

    void writeChildren(AstNode const& node) {
        fmt.writeChildren(node.children(),
                          [&](size_t index, AstNode const* child) {
            auto label = get(get(labelMap, get_rtti(node), {}), index, {});
            if (!label.empty()) {
                str << Secondary(label, ": ");
            }
            dfs(child);
        });
    }

    void writeChildren(std::derived_from<FacetPlaceholder> auto const& node) {
        fmt.writeChild([&] { print(node.facet(), str, fmt, ctx); });
    }

    void endNode(AstNode const&) {}

    void label(AstSourceFile const& node) {
        str << Path(node.sourceContext().filepath());
        ctx = &node.sourceContext();
    }

    void endNode(AstSourceFile const&) { ctx = nullptr; }

    void label(AstArithmeticExpr const& expr) { str << Op(expr.operation()); }

    void label(AstUnqualName const& expr) {
        if (ctx) {
            str << tfmt::format(Underline, ctx->getTokenStr(expr.nameToken()));
        }
    }
};

} // namespace

void prism::dumpAst(AstNode const* root, std::ostream& str) {
    TreeFormatter fmt(str);
    dumpAst(root, str, fmt);
}

void prism::dumpAst(AstNode const* root, std::ostream& str,
                    TreeFormatter& fmt) {
    Emitter(str, fmt).run(root);
}
