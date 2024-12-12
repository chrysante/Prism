#include <concepts>
#include <initializer_list>
#include <ostream>
#include <span>
#include <variant>
#include <vector>

#include <Prism/Ast/Ast.h>
#include <Prism/Ast/AstDump.h>
#include <Prism/Ast/Facet.h>
#include <Prism/Common/Allocator.h>
#include <Prism/Source/Token.h>

namespace prism {

namespace internal {

extern MonotonicBufferResource gAlloc;

}

class Issue;
class IssueHandler;

inline std::ostream& operator<<(std::ostream& str, AstNode const& node) {
    dumpAst(&node, str);
    return str;
}

inline std::ostream& operator<<(std::ostream& str, Facet const& facet) {
    print(&facet, str);
    return str;
}

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

using VarType = std::variant<AstNodeType, FacetType, TokenKind, NullNodeT>;

/// Reference tree node for simple and consice testing of parsed ASTs and facet
/// trees
class AstRefNode {
public:
    AstRefNode(VarType type, std::span<AstRefNode const* const> children):
        type(type), children(children), expectedIssues(&internal::gAlloc) {}

private:
    friend bool operator==(AstNode const& node, AstRefNode const* ref) {
        return ref->compare(&node) && ref->verifyIssues();
    }

    friend bool operator==(Facet const& facet, AstRefNode const* ref) {
        return ref->compare(&facet) && ref->verifyIssues();
    }

    friend AstRefNode* operator>>(AstRefNode* node, ExpectedIssue const& e);

    bool compare(AstNode const* node) const;

    bool compare(Facet const* node) const;

    template <typename T>
    bool checkType(T t) const;

    bool compareChildren(auto const* node) const;

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

AstSourceFile* parseFile(std::string_view text);

Facet const* parseFacet(std::string_view text);

} // namespace prism
