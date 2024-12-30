#include "Prism/Parser/ParserTestUtils.h"

#include <tuple>
#include <unordered_map>
#include <vector>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/hash.hpp>
#include <utl/hashtable.hpp>
#include <utl/strcat.hpp>

#include <Prism/Common/DiagnosticHandler.h>
#include <Prism/Common/Functional.h>
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
std::vector<std::function<void(std::ostream&)>> gDiagnosticMatchErrors;

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
    for (auto& err: gDiagnosticMatchErrors)
        err(str);
    return str;
}

namespace prism {

static std::ostream& operator<<(std::ostream& str, VarType const& type) {
    // clang-format off
    std::visit(csp::overload{
        [&](FacetType type) { str << type; },
        [&](TokenKind type) { str << type; },
        [&](NullNodeT) { str << "null"; }
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
static DiagnosticHandler gDiagnosticHandler;

static bool matchDiagnostic(ExpectedDiagnostic const& expIss,
                            utl::hashset<Diagnostic const*>& diags) {
    for (auto itr = diags.begin(); itr != diags.end(); ++itr) {
        auto& diag = **itr;
        if (!expIss.checkType(&diag)) continue;
        auto sourceLoc = diag.sourceRange().value().begin;
        if (sourceLoc.line != expIss.line) continue;
        if (expIss.column && sourceLoc.column != *expIss.column) continue;
        diags.erase(itr);
        return true;
    }
    return false;
}

bool AstRefNode::verifyDiagnostics() const {
    auto diags = gDiagnosticHandler | ranges::views::transform(AddressOf) |
                 ranges::to<utl::hashset<Diagnostic const*>>;
    bool result = true;
    for (auto* expDiagnostic: expectedDiagnostics) {
        if (!matchDiagnostic(*expDiagnostic, diags)) {
            result = false;
            gDiagnosticMatchErrors.push_back([=](std::ostream& str) {
                str << "Failed to match expected diag: " << *expDiagnostic
                    << std::endl;
            });
        }
    }
    for (auto* diag: diags) {
        gDiagnosticMatchErrors.push_back([=](std::ostream& str) {
            str << "Unexpected diag: ";
            diag->format(str, &gCtx);
            str << std::endl;
        });
    }
    return result && diags.empty();
}

bool prism::operator==(Facet const& facet, AstRefNode const* ref) {
    gErrorMap.clear();
    gDiagnosticMatchErrors.clear();
    return ref->compare(&facet, nullptr, 0) && ref->verifyDiagnostics();
}

AstRefNode* prism::operator>>(AstRefNode* node, ExpectedDiagnostic const& e) {
    node->expectedDiagnostics.push_back(&e);
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
    gDiagnosticHandler.clear();
    return parseSourceFile(gAlloc, gCtx, gDiagnosticHandler);
}

Facet const* prism::parseExpr(std::string_view text) {
    gCtx = SourceContext({}, text);
    gDiagnosticHandler.clear();
    return parseExpr(gAlloc, gCtx, gDiagnosticHandler);
}

Facet const* prism::parseTypeSpec(std::string_view text) {
    gCtx = SourceContext({}, text);
    gDiagnosticHandler.clear();
    return parseTypeSpec(gAlloc, gCtx, gDiagnosticHandler);
}
