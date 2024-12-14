#include "Prism/Parser/ParserTestUtils.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include <Prism/Common/IssueHandler.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;

bool ExpectedIssue::verify(IssueHandler const& iss,
                           SourceContext const& ctx) const {
    for (auto& issue: iss) {
        if (!checkType(&issue)) continue;
        auto sourceLoc = ctx.getSourceLocation(issue.sourceIndex());
        if (sourceLoc.line != line) continue;
        if (column && sourceLoc.column != *column) continue;
        return true;
    }
    return false;
}

template <typename T>
static T dyncast_ext(auto* p) {
    if (!p) return nullptr;
    using U = std::remove_cv_t<std::remove_pointer_t<T>>;
    return csp::visit(*p, csp::overload{ [](auto&) -> T {
        return nullptr;
    }, [](std::derived_from<U> auto& u) -> T { return &u; } });
}

bool AstRefNode::compare(AstNode const* node) const {
    if (!node) return std::holds_alternative<NullNodeT>(type);
    if (auto* facetNode = dyncast_ext<RawFacetBase const*>(node)) {
        return compare(facetNode->facet());
    }
    return checkType(get_rtti(*node)) && compareChildren(node);
}

bool AstRefNode::compare(Facet const* facet) const {
    if (!facet) return std::holds_alternative<NullNodeT>(type);
    if (auto* term = csp::dyncast<TerminalFacet const*>(facet)) {
        return checkType(term->token().kind);
    }
    return checkType(get_rtti(*facet)) && compareChildren(facet);
}

template <typename T>
bool AstRefNode::checkType(T t) const {
    auto* u = std::get_if<T>(&type);
    return u && *u == t;
}

bool AstRefNode::compareChildren(auto const* node) const {
    if (!children.empty() && children.size() != node->children().size())
        return false;
    return ranges::all_of(ranges::views::zip(children, node->children()),
                          [](auto&& p) {
        auto [ref, node] = p;
        return ref->compare(node);
    });
}

MonotonicBufferResource internal::gAlloc;
using internal::gAlloc;
static SourceContext gCtx;
static IssueHandler gIssueHandler;

bool AstRefNode::verifyIssues() const {
    return ranges::all_of(expectedIssues, [](auto* e) {
        return e->verify(gIssueHandler, gCtx);
    });
}

AstRefNode* prism::operator>>(AstRefNode* node, ExpectedIssue const& e) {
    node->expectedIssues.push_back(&e);
    return node;
}

static AstRefNode* allocateNode(
    VarType type, std::span<AstRefNode const* const> children = {}) {
    return allocate<AstRefNode>(gAlloc, type, children);
}

static AstRefNode const* toNode(std::variant<AstRefNode const*, VarType> v) {
    csp::overload visitor{
        [](AstRefNode const* p) { return p; },
        [](auto type) -> AstRefNode const* { return allocateNode(type); },
    };
    return std::visit(visitor, v);
}

Tree::Tree(
    std::initializer_list<std::variant<AstRefNode const*, VarType>> children) {
    auto t = children | ranges::views::transform(toNode);
    value = allocateArray<AstRefNode const*>(gAlloc, t.begin(), t.end());
};

AstRefNode* prism::operator>>(VarType type, Tree children) {
    return allocateNode(type, children.value);
}

AstSourceFile* prism::parseFile(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseSourceFile(gAlloc, gCtx, gIssueHandler);
}

Facet const* prism::parseFacet(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseFacet(gAlloc, gCtx, gIssueHandler);
}
