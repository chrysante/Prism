#include "Prism/Ast/AstDump.h"

#include <termfmt/termfmt.h>
#include <utl/hashtable.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Ast/Ast.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using namespace tfmt::modifiers;

static constexpr utl::streammanip NullNode = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

static constexpr utl::streammanip NodeType = [](std::ostream& str,
                                                AstNode const& node) {
    str << tfmt::format(BrightGreen | Italic, get_rtti(node));
};

static constexpr utl::streammanip Secondary = [](std::ostream& str,
                                                 auto const&... args) {
    str << tfmt::format(BrightGrey, args...);
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

struct DumpContext {
    SourceContext const* ctx = nullptr;
    std::ostream& str;
    TreeFormatter fmt;
    utl::hashmap<AstNodeType, utl::hashmap<size_t, std::string>> labelMap;

    DumpContext(std::ostream& str):
        str(str),
        fmt(str),
        labelMap(
            { { AstNodeType::AstParamDecl, { { 0, "Name" }, { 1, "Type" } } },
              { AstNodeType::AstFuncDecl,
                { { 0, "Name" }, { 2, "RetType" }, { 3, "Body" } } } }) {}

    void run(AstNode const* root) { dfs(root); }

    void dfs(AstNode const* node) {
        if (!node) {
            str << NullNode << "\n";
            return;
        }
        csp::visit(*node, [&](auto& node) {
            writeNode(node);
            writeChildren(node);
            endNode(node);
        });
    }

    void writeNode(AstNode const& node) { str << NodeType(node) << "\n"; }

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

    void endNode(AstNode const&) {}

    void writeNode(AstSourceFile const& node) {
        str << NodeType(node) << " " << Path(node.sourceContext().filepath())
            << "\n";
        ctx = &node.sourceContext();
    }

    void endNode(AstSourceFile const&) { ctx = nullptr; }

    void writeNode(AstUnqualName const& node) {
        str << NodeType(node);
        if (ctx) {
            str << " \"" << ctx->getTokenStr(node.nameToken()) << "\"";
        }
        str << "\n";
    }
};

} // namespace

void prism::dumpAst(AstNode const* root, std::ostream& str) {
    DumpContext(str).run(root);
}
