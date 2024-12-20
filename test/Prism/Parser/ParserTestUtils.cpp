#include "Prism/Parser/ParserTestUtils.h"

#include <tuple>
#include <unordered_map>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/hash.hpp>
#include <utl/strcat.hpp>

#include <Prism/Common/IssueHandler.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

using namespace prism;
using ranges::views::iota;
using ranges::views::zip;

static bool getEnvVarBool(char const* name) {
    auto* value = std::getenv(name);
    if (!value) return false;
    static constexpr std::string_view Yes[] = {
        "YES", "Y", "Yes", "yes", "y", "TRUE", "True", "true",
    };
    return ranges::contains(Yes, std::string_view(value));
}

static bool const BreakOnTreeMismatch = getEnvVarBool("BREAK_TREE_MISMATCH");

namespace {

struct TreeErrorKey {
    Facet const* node;
    Facet const* parent;
    size_t index;

    bool operator==(TreeErrorKey const&) const = default;
};

} // namespace

template <>
struct std::hash<TreeErrorKey> {
    size_t operator()(TreeErrorKey const& key) const {
        return utl::hash_combine(key.node, key.parent, key.index);
    }
};

static std::unordered_map<TreeErrorKey, std::string> gErrorMap;

static bool validate(bool value, TreeErrorKey key, auto&&... message) {
    if (value) return true;
    gErrorMap.insert({ key, utl::strcat(message...) });
    if (BreakOnTreeMismatch) __builtin_debugtrap();
    return false;
}

std::ostream& prism::operator<<(std::ostream& str, Facet const& facet) {
    auto callback = [](std::ostream& str, Facet const* facet,
                       Facet const* parent, size_t index) {
        auto itr = gErrorMap.find({ facet, parent, index });
        if (itr != gErrorMap.end()) {
            auto& [key, value] = *itr;
            str << " <- " << value;
        }
    };
    print(&facet, str, { .nodeCallback = callback });
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

namespace prism {

static std::ostream& operator<<(std::ostream& str, VarType const& type) {
    // clang-format off
    std::visit(csp::overload{
        [&](FacetType type) { str << type; },
        [&](TokenKind type) { str << type; },
        [&](NullNodeT type) { str << "null"; }
    }, type); // clang-format on
    return str;
}

} // namespace prism

bool AstRefNode::compare(Facet const* facet, Facet const* parent,
                         size_t index) const {
    TreeErrorKey key{ facet, parent, index };
    if (!facet)
        return validate(std::holds_alternative<NullNodeT>(type), key,
                        "Expected ", type);
    if (auto* term = dyncast<TerminalFacet const*>(facet))
        return validate(checkType(term->token().kind), key,
                        utl::strcat("Expected ", type, " token"));
    bool result = validate(checkType(get_rtti(*facet)), key,
                           utl::strcat("Expected ", type));
    result &= compareChildren(facet, parent, index);
    return result;
}

template <typename T>
bool AstRefNode::checkType(T t) const {
    auto* u = std::get_if<T>(&type);
    return u && *u == t;
}

bool AstRefNode::compareChildren(Facet const* node, Facet const* parent,
                                 size_t index) const {
    if (!children.empty())
        validate(children.size() == node->children().size(),
                 { node, parent, index }, "Invalid number of children");
    bool result = true;
    for (auto [childRef, childFacet, index]:
         zip(children, node->children(), iota(0u)))
    {
        result &= childRef->compare(childFacet, node, index);
    }
    return result;
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

bool prism::operator==(Facet const& facet, AstRefNode const* ref) {
    gErrorMap.clear();
    return ref->compare(&facet, nullptr, 0) && ref->verifyIssues();
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

Facet const* prism::parseExpr(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseExpr(gAlloc, gCtx, gIssueHandler);
}

Facet const* prism::parseTypeSpec(std::string_view text) {
    gCtx = SourceContext({}, text);
    gIssueHandler.clear();
    return parseTypeSpec(gAlloc, gCtx, gIssueHandler);
}
