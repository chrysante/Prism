#include <concepts>
#include <initializer_list>
#include <span>
#include <variant>

#include <Prism/Ast/Ast.h>
#include <Prism/Ast/Facet.h>
#include <Prism/Source/Token.h>

namespace prism {

using VarType = std::variant<AstNodeType, FacetType, TokenKind>;

/// Reference tree node for simple and consice testing of parsed ASTs and facet
/// trees
class AstRefNode {
public:
    AstRefNode(VarType type, std::span<AstRefNode const* const> children):
        type(type), children(children) {}

private:
    friend bool operator==(AstNode const& node, AstRefNode const* ref) {
        return ref->compare(&node);
    }

    friend bool operator==(Facet const& facet, AstRefNode const* ref) {
        return ref->compare(&facet);
    }

    bool compare(AstNode const* node) const;

    bool compare(Facet const* node) const;

    VarType type;
    std::span<AstRefNode const* const> children;
};

/// Helper class for concise construction of reference trees
class Tree {
public:
    Tree(std::initializer_list<std::variant<AstRefNode const*, VarType>>
             children);

    friend AstRefNode const* operator>>(VarType type, Tree children);

private:
    std::span<AstRefNode const* const> value;
};

AstRefNode const* operator>>(std::convertible_to<VarType> auto type,
                             std::convertible_to<VarType> auto child) {
    return type >> Tree{ child };
}

csp::unique_ptr<AstSourceFile> parseFile(std::string_view text);

Facet const* parseFacet(std::string_view text);

} // namespace prism
