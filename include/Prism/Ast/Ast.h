#ifndef PRISM_AST_AST_H
#define PRISM_AST_AST_H

#include <optional>
#include <span>

#include <csp.hpp>
#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include <Prism/Ast/Operators.h>
#include <Prism/Common/Functional.h>
#include <Prism/Source/Token.h>

#define AST_COMMON(NodeName) friend class AstNode;

#define AST_PROPERTY(Index, Type, Name, CapName)                               \
    Type* Name() { return childAt<Type>(Index); }                              \
    Type const* Name() const { return childAt<Type>(Index); }

#define AST_PROPERTY_RANGE(BeginIndex, Type, Name, CapName)                    \
    auto Name##s() {                                                           \
        return children() | ranges::views::drop(BeginIndex) |                  \
               ranges::views::transform(csp::cast<Type*>);                     \
    }                                                                          \
    auto Name##s() const {                                                     \
        return children() | ranges::views::drop(BeginIndex) |                  \
               ranges::views::transform(csp::cast<Type const*>);               \
    }

namespace prism {

enum class AstNodeType {
#define AST_NODE(Type, ...) Type,
#include <Prism/Ast/Ast.def>
};

#define AST_NODE(Type, ...) class Type;
#include <Prism/Ast/Ast.def>

namespace detail {

using NoParent = void;

}

} // namespace prism

#define AST_NODE(Type, Parent, Corporeality)                                   \
    CSP_DEFINE(prism::Type, prism::AstNodeType::Type, prism::Parent,           \
               Corporeality)
#include <Prism/Ast/Ast.def>

namespace prism {

/// MARK: - Base nodes

/// Base class of all AST nodes
class AstNode: public csp::base_helper<AstNode> {
public:
    Token firstToken() const { return firstTok; }

    AstNode* childAt(size_t index) { return _children[index].get(); }

    AstNode const* childAt(size_t index) const {
        return _children[index].get();
    }

    template <typename T>
    T* childAt(size_t index) {
        return csp::cast<T*>(childAt(index));
    }

    template <typename T>
    T const* childAt(size_t index) const {
        return csp::cast<T const*>(childAt(index));
    }

    auto children() {
        return _children | ranges::views::transform(GetAs<AstNode*>);
    }

    auto children() const {
        return _children | ranges::views::transform(GetAs<AstNode const*>);
    }

protected:
    template <typename... C>
    explicit AstNode(AstNodeType type, Token firstToken, C&&... children):
        base_helper(type),
        firstTok(firstToken),
        _children(toSmallVec(std::forward<C>(children)...)) {}

private:
    static constexpr size_t NumInlineChildren = 3;

    template <typename T>
    static size_t vecSize(T const& child) {
        if constexpr (ranges::range<T>) {
            return ranges::size(child);
        }
        else {
            return 1;
        }
    }

    template <typename T>
    static void insertChildren(utl::vector<csp::unique_ptr<AstNode>>& vec,
                               T&& child) {
        if constexpr (ranges::range<T>) {
            ranges::for_each(child, [&](auto& elem) {
                insertChildren(vec, std::move(elem));
            });
        }
        else {
            vec.push_back(std::move(child));
        }
    }

    template <typename... T>
    static utl::small_vector<csp::unique_ptr<AstNode>, NumInlineChildren>
        toSmallVec(T&&... children) {
        utl::small_vector<csp::unique_ptr<AstNode>, NumInlineChildren> result;
        result.reserve((0 + ... + vecSize(children)));
        (insertChildren(result, std::forward<T>(children)), ...);
        return result;
    }

    Token firstTok;
    utl::small_vector<csp::unique_ptr<AstNode>, 2> _children;
};

/// Abstract base class of AST expressions
class AstExpr: public AstNode {
protected:
    using AstNode::AstNode;
};

///
class AstName: public AstExpr {
protected:
    using AstExpr::AstExpr;
};

/// Unqualified name consisting of a single token
class AstUnqualName: public AstName {
public:
    explicit AstUnqualName(Token name):
        AstName(AstNodeType::AstUnqualName, name) {}

    Token nameToken() const { return firstToken(); }
};

/// Qualified name, consisting of a sequence of tokens separated by `.`
class AstNameSeq: public AstName {
public:
    explicit AstNameSeq(std::span<Token const> seq):
        AstName(AstNodeType::AstNameSeq, seq.front()),
        _seq(seq.begin(), seq.end()) {}

    std::span<Token const> tokenSeq() const { return _seq; }

private:
    utl::small_vector<Token, 3> _seq;
};

/// Base class of all AST statements
class AstStmt: public AstNode {
protected:
    using AstNode::AstNode;
};

/// Brace-enclosed sequence of statements
class AstCompoundStmt: public AstStmt {
public:
    explicit AstCompoundStmt(Token openBrace, Token closeBrace,
                             utl::small_vector<csp::unique_ptr<AstStmt>> stmts):
        AstStmt(AstNodeType::AstCompoundStmt, openBrace, std::move(stmts)) {}

    Token openBrace() const { return firstToken(); }

    Token closeBrace() const { return _closeBrace; }

    /// The sequence of statements
    AST_PROPERTY_RANGE(0, AstStmt, statement, Statement)

private:
    Token _closeBrace;
};

/// Base class of all AST declarations
class AstDecl: public AstStmt {
public:
    /// The keyword introducing this declaration, like `let`, `fn` or `struct`
    Token declarator() const { return _declarator; }

    /// The name declared by this declaration
    AST_PROPERTY(0, AstName, name, Name)

protected:
    template <typename... C>
    explicit AstDecl(AstNodeType type, Token declarator,
                     csp::unique_ptr<AstName> name, C&&... otherChildren):
        AstStmt(type, name->firstToken(), std::move(name),
                std::forward<C>(otherChildren)...),
        _declarator(declarator) {}

private:
    Token _declarator;
};

/// List of declarations in a source file
class AstSourceFile: public AstNode {
public:
    AST_COMMON(AstSourceFile)

    /// The top level declarations
    AST_PROPERTY_RANGE(0, AstDecl, decl, Decl)

    explicit AstSourceFile(utl::small_vector<csp::unique_ptr<AstDecl>> decls):
        AstNode(AstNodeType::AstSourceFile, Token{}, std::move(decls)) {}
};

/// Root node of the tree. Contains all global declarations
class AstTranslationUnit: public AstNode {
public:
    AST_COMMON(AstTranslationUnit)

    /// The source files in this translation unit
    AST_PROPERTY_RANGE(0, AstSourceFile, sourceFile, SourceFile)

    explicit AstTranslationUnit(
        utl::small_vector<csp::unique_ptr<AstSourceFile>> sourceFiles):
        AstNode(AstNodeType::AstTranslationUnit, Token{},
                std::move(sourceFiles)) {}
};

/// MARK: - Expressions

/// Arithmetic expression
class AstArithmeticExpr: public AstExpr {
public:
    explicit AstArithmeticExpr(Token firstToken, AstArithmeticOp operation):
        AstExpr(AstNodeType::AstArithmeticExpr, firstToken), op(operation) {}

    AstArithmeticOp operation() const { return op; }

private:
    AstArithmeticOp op;
};

/// Assignment expression, possibly an arithmetic assignment
class AstAssignExpr: public AstExpr {
public:
    explicit AstAssignExpr(
        Token firstToken,
        std::optional<AstArithmeticOp> operation = std::nullopt):
        AstExpr(AstNodeType::AstAssignExpr, firstToken),
        op(operation.value_or(AstArithmeticOp(-1))) {}

    /// \Returns an `AstArithmeticOp` if this is an
    /// arithmetic-assign-expression, otherwise nullopt
    std::optional<AstArithmeticOp> arithmeticOperation() const {
        return op == AstArithmeticOp(-1) ? std::optional<AstArithmeticOp>{} :
                                           op;
    }

private:
    AstArithmeticOp op;
};

/// MARK: - Statements

/// Expression statement
class AstExprStmt: public AstStmt {
public:
    explicit AstExprStmt(csp::unique_ptr<AstExpr> expr):
        AstStmt(AstNodeType::AstExprStmt, expr->firstToken(), std::move(expr)) {
    }

    /// The wrapped expression
    AST_PROPERTY(0, AstExpr, expr, Expr)
};

///
class AstParamDecl: public AstDecl {
public:
    explicit AstParamDecl(csp::unique_ptr<AstUnqualName> name, Token colon,
                          csp::unique_ptr<AstExpr> typeSpec):
        AstDecl(AstNodeType::AstParamDecl, name->firstToken(), std::move(name),
                std::move(typeSpec)) {}

    AST_PROPERTY(1, AstExpr, typeSpec, TypeSpec)

    Token colon() const { return _colon; }

private:
    Token _colon;
};

/// Comma separated sequence of parameter declarations
class AstParamList: public AstNode {
public:
    explicit AstParamList(
        Token openParen, Token closeParen,
        utl::small_vector<csp::unique_ptr<AstParamDecl>> params):
        AstNode(AstNodeType::AstParamList, openParen, std::move(params)),
        _closeParen(closeParen) {}

    AST_PROPERTY_RANGE(0, AstParamDecl, param, Param)

    Token openParen() const { return firstToken(); }

    Token closeParen() const { return _closeParen; }

private:
    Token _closeParen;
};

/// Function declaration or definition
class AstFuncDecl: public AstDecl {
public:
    AST_COMMON(AstFuncDecl);

    AST_PROPERTY(1, AstParamList, params, Params)

    AST_PROPERTY(2, AstExpr, retTypeSpec, RetTypeSpec)

    AST_PROPERTY(3, AstCompoundStmt, body, Body)

    explicit AstFuncDecl(Token declarator, csp::unique_ptr<AstName> name,
                         csp::unique_ptr<AstParamList> params,
                         csp::unique_ptr<AstExpr> retTypeSpec,
                         csp::unique_ptr<AstCompoundStmt> body):
        AstDecl(AstNodeType::AstFuncDecl, declarator, std::move(name),
                std::move(params), std::move(retTypeSpec), std::move(body)) {}
};

} // namespace prism

#undef AST_COMMON
#undef AST_PROPERTY
#undef AST_PROPERTY_RANGE

#endif // PRISM_AST_AST_H
