#include "Prism/Parser/ParserTestUtils.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include <Prism/Common/IssueHandler.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;

std::ostream& prism::operator<<(std::ostream& str, Facet const& facet) {
    print(&facet, str);
    return str;
}

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

static bool getEnvVarBool(char const* name) {
    auto* value = std::getenv(name);
    if (!value) return false;
    static constexpr std::string_view Yes[] = {
        "YES", "Y", "Yes", "yes", "y", "TRUE", "True", "true",
    };
    return ranges::contains(Yes, std::string_view(value));
}

static bool const BreakOnTreeMismatch = getEnvVarBool("BREAK_TREE_MISMATCH");

#define VALIDATE(expr, ...)                                                    \
    ((expr) ?                                                                  \
         true :                                                                \
         ((BreakOnTreeMismatch ? __builtin_debugtrap() : (void)0), false))

bool AstRefNode::compare(Facet const* facet) const {
    if (!facet) return VALIDATE(std::holds_alternative<NullNodeT>(type));
    if (auto* term = dyncast<TerminalFacet const*>(facet))
        return checkType(term->token().kind);
    return checkType(get_rtti(*facet)) && compareChildren(facet);
}

template <typename T>
bool AstRefNode::checkType(T t) const {
    auto* u = std::get_if<T>(&type);
    return VALIDATE(u && *u == t);
}

bool AstRefNode::compareChildren(Facet const* facet) const {
    if (!children.empty())
        if (!VALIDATE(children.size() == facet->children().size()))
            return false;
    return ranges::all_of(ranges::views::zip(children, facet->children()),
                          [](auto&& p) {
        auto [childRef, childFacet] = p;
        return childRef->compare(childFacet);
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

SourceFileFacet const* prism::parseFile(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseSourceFile(gAlloc, gCtx, gIssueHandler);
}

Facet const* prism::parseFacet(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseFacet(gAlloc, gCtx, gIssueHandler);
}
