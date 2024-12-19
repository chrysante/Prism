#ifndef PRISM_PARSER_PARSERTESTUTILS_H
#define PRISM_PARSER_PARSERTESTUTILS_H

#include <concepts>
#include <initializer_list>
#include <optional>
#include <ostream>
#include <span>
#include <variant>
#include <vector>

#include <Prism/Common/Allocator.h>
#include <Prism/Facet/Facet.h>
#include <Prism/Source/Token.h>

namespace prism {

namespace internal {

extern MonotonicBufferResource gAlloc;

}

class Issue;
class IssueHandler;

std::ostream& operator<<(std::ostream& str, Facet const& facet);

class ExpectedIssue {
public:
    virtual ~ExpectedIssue() = default;

    bool verify(IssueHandler const& iss, SourceContext const& ctx) const;

protected:
    ExpectedIssue(uint32_t line, std::optional<uint32_t> column):
        line(line), column(column) {}

    uint32_t line;
    std::optional<uint32_t> column;

private:
    virtual bool checkType(Issue const* issue) const = 0;
};

template <typename IssueType>
class ExpectedIssueImpl: public ExpectedIssue {
public:
    explicit ExpectedIssueImpl(uint32_t line, std::optional<uint32_t> column):
        ExpectedIssue(line, column) {}

private:
    bool checkType(Issue const* issue) const override {
        return dynamic_cast<IssueType const*>(issue) != nullptr;
    }
};

template <std::derived_from<Issue> IssueType>
ExpectedIssue const& IssueOnLine(
    uint32_t line, std::optional<uint32_t> column = std::nullopt) {
    return *allocate<ExpectedIssueImpl<IssueType>>(internal::gAlloc, line,
                                                   column);
}

enum class NullNodeT : int;

inline constexpr NullNodeT NullNode{};

struct VarType: std::variant<FacetType, TokenKind, NullNodeT> {
    using variant::variant;
};

/// Reference tree node for simple and consice testing of parsed ASTs and facet
/// trees
class AstRefNode {
public:
    AstRefNode(VarType type, std::span<AstRefNode const* const> children):
        type(type), children(children), expectedIssues(&internal::gAlloc) {}

private:
    friend bool operator==(Facet const& facet, AstRefNode const* ref);

    friend AstRefNode* operator>>(AstRefNode* node, ExpectedIssue const& e);

    bool compare(Facet const* node, Facet const* parent, size_t index) const;

    template <typename T>
    bool checkType(T t) const;

    bool compareChildren(Facet const* node, Facet const* parent,
                         size_t index) const;

    bool verifyIssues() const;

    VarType type;
    std::span<AstRefNode const* const> children;
    std::vector<
        ExpectedIssue const*,
        ResourceAllocator<ExpectedIssue const*, MonotonicBufferResource>>
        expectedIssues;
};

/// Helper class for concise construction of reference trees
class Tree {
public:
    Tree(std::initializer_list<std::variant<AstRefNode const*, VarType>>
             children);

    friend AstRefNode* operator>>(VarType type, Tree children);

private:
    std::span<AstRefNode const* const> value;
};

AstRefNode* operator>>(std::convertible_to<VarType> auto type,
                       std::convertible_to<VarType> auto child) {
    return type >> Tree{ child };
}

SourceFileFacet const* parseFile(std::string_view text);

Facet const* parseFacet(std::string_view text);

} // namespace prism

#endif // PRISM_PARSER_PARSERTESTUTILS_H
