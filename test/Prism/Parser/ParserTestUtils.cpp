#include "Prism/Parser/ParserTestUtils.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include <Prism/Common/IssueHandler.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;

bool AstRefNode::compare(AstNode const* node) const {
    if (auto* facetNode = csp::dyncast<AstExprFacet const*>(node)) {
        return compare(facetNode->facet());
    }
    if (auto* facetNode = csp::dyncast<AstTypeSpecFacet const*>(node)) {
        return compare(facetNode->facet());
    }
    if (!node) return false;
    auto* t = std::get_if<AstNodeType>(&type);
    if (!t) return false;
    if (get_rtti(*node) != *t) return false;
    if (!children.empty() && children.size() != node->children().size())
        return false;
    return ranges::all_of(ranges::views::zip(children, node->children()),
                          [](auto&& p) {
        auto [ref, node] = p;
        return ref->compare(node);
    });
}

bool AstRefNode::compare(Facet const* node) const {
    if (!node) return false;
    if (auto* term = csp::dyncast<TerminalFacet const*>(node)) {
        auto* t = std::get_if<TokenKind>(&type);
        return t && term->token().kind == *t;
    }
    auto* t = std::get_if<FacetType>(&type);
    return t && get_rtti(*node) == *t;
}

static MonotonicBufferAllocator gAlloc;

static AstRefNode const* allocateNode(
    VarType type, std::span<AstRefNode const* const> children = {}) {
    return allocate<AstRefNode>(gAlloc, type, children);
}

static AstRefNode const* toNode(std::variant<AstRefNode const*, VarType> v) {
    csp::overload visitor{
        [](AstRefNode const* p) { return p; },
        [](auto type) { return allocateNode(type); },
    };
    return std::visit(visitor, v);
}

Tree::Tree(
    std::initializer_list<std::variant<AstRefNode const*, VarType>> children) {
    auto t = children | ranges::views::transform(toNode);
    value = allocateArray<AstRefNode const*>(gAlloc, t.begin(), t.end());
};

AstRefNode const* prism::operator>>(VarType type, Tree children) {
    return allocateNode(type, children.value);
}

static SourceContext gCtx;
static IssueHandler gIssueHandler;

csp::unique_ptr<AstSourceFile> prism::parseFile(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseSourceFile(gAlloc, gCtx, gIssueHandler);
}

Facet const* prism::parseFacet(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseFacet(gAlloc, gCtx, gIssueHandler);
}
