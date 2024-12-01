#ifndef PRISM_AST_AST_H
#define PRISM_AST_AST_H

#include <optional>
#include <span>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include <Prism/Ast/AstFwd.h>
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

class SourceContext;
class Facet;

// MARK: - Base nodes

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
    static constexpr size_t NumInlineChildren = 2;

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
    utl::small_vector<csp::unique_ptr<AstNode>, NumInlineChildren> _children;
};

// MARK: - Facets

/// Base class of expressions and type specifiers
class AstFacet: public AstNode {
protected:
    using AstNode::AstNode;
};

/// Facet wrapper for an expression or type specifier
class RawFacetBase {
public:
    explicit RawFacetBase(Facet const* fct): fct(fct) {}

    /// \Returns the raw source facet
    Facet const* facet() const { return fct; }

private:
    Facet const* fct;
};

///
class AstRawFacet: public AstFacet, public RawFacetBase {
public:
    explicit AstRawFacet(Facet const* fct, Token firstTok):
        AstFacet(AstNodeType::AstRawFacet, firstTok), RawFacetBase(fct) {}
};

// MARK: - Base Expressions

/// Abstract base class of AST expressions
class AstExpr: public AstFacet {
protected:
    using AstFacet::AstFacet;
};

///
class AstExprFacet: public AstExpr, public RawFacetBase {
public:
    explicit AstExprFacet(Facet const* fct, Token firstTok):
        AstExpr(AstNodeType::AstExprFacet, firstTok), RawFacetBase(fct) {}
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

// MARK: - Type Specifiers

/// Base class of all type specifiers
class AstTypeSpec: public AstFacet {
protected:
    using AstFacet::AstFacet;
};

///
class AstTypeSpecFacet: public AstTypeSpec, public RawFacetBase {
public:
    explicit AstTypeSpecFacet(Facet const* fct, Token firstTok):
        AstTypeSpec(AstNodeType::AstTypeSpecFacet, firstTok),
        RawFacetBase(fct) {}
};

class AstUnaryTypeSpec: public AstTypeSpec {};

class AstTypeRef: public AstUnaryTypeSpec {};

class AstTypePtr: public AstUnaryTypeSpec {};

class AstTypeOpt: public AstUnaryTypeSpec {};

///
class AstTypeID: public AstTypeSpec {
protected:
    using AstTypeSpec::AstTypeSpec;
};

///
class AstTypeUnqualID: public AstTypeID {
public:
    AstTypeUnqualID(Token tok): AstTypeID(AstNodeType::AstTypeUnqualID, tok) {}

    Token nameToken() const { return firstToken(); }
};

// MARK: - Statements

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
    Token declarator() const { return firstToken(); }

    /// The name declared by this declaration
    AST_PROPERTY(0, AstName, name, Name)

protected:
    template <typename... C>
    explicit AstDecl(AstNodeType type, Token declarator,
                     csp::unique_ptr<AstName> name, C&&... otherChildren):
        AstStmt(type, declarator, std::move(name),
                std::forward<C>(otherChildren)...) {}
};

/// List of declarations in a source file
class AstSourceFile: public AstNode {
public:
    AST_COMMON(AstSourceFile)

    /// The top level declarations
    AST_PROPERTY_RANGE(0, AstDecl, decl, Decl)

    explicit AstSourceFile(SourceContext const& ctx,
                           utl::small_vector<csp::unique_ptr<AstDecl>> decls):
        AstNode(AstNodeType::AstSourceFile, Token{}, std::move(decls)),
        ctx(ctx) {}

    /// \Returns the source context corresponding this source file
    SourceContext const& sourceContext() const { return ctx; }

private:
    SourceContext const& ctx;
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

// MARK: - Expressions

/// Arithmetic expression
class AstBinaryExpr: public AstExpr {
public:
    AST_PROPERTY(0, AstExpr, LHS, LHS)

    AST_PROPERTY(1, AstExpr, RHS, RHS)

    Token opToken() const { return opTok; }

protected:
    explicit AstBinaryExpr(AstNodeType nodeType, Token opToken,
                           csp::unique_ptr<AstExpr> lhs,
                           csp::unique_ptr<AstExpr> rhs):
        AstExpr(nodeType, lhs->firstToken(), std::move(lhs), std::move(rhs)),
        opTok(opToken) {}

private:
    Token opTok;
};

class AstCommaExpr: public AstBinaryExpr {
public:
    explicit AstCommaExpr(Token commaToken, csp::unique_ptr<AstExpr> lhs,
                          csp::unique_ptr<AstExpr> rhs):
        AstBinaryExpr(AstNodeType::AstCommaExpr, commaToken, std::move(lhs),
                      std::move(rhs)) {}
};

class AstAssignExpr: public AstBinaryExpr {
public:
    explicit AstAssignExpr(Token opToken, csp::unique_ptr<AstExpr> lhs,
                           csp::unique_ptr<AstExpr> rhs):
        AstBinaryExpr(AstNodeType::AstAssignExpr, opToken, std::move(lhs),
                      std::move(rhs)) {}
};

class AstCastExpr: public AstExpr {
public:
    explicit AstCastExpr(Token opToken, csp::unique_ptr<AstExpr> operand,
                         csp::unique_ptr<AstTypeSpec> targetType):
        AstExpr(AstNodeType::AstCastExpr, operand->firstToken(),
                std::move(operand), std::move(targetType)),
        opTok(opToken) {}

    AST_PROPERTY(0, AstExpr, operand, Operand)

    AST_PROPERTY(1, AstTypeSpec, targetType, TargetType)

    Token opToken() const { return opTok; }

private:
    Token opTok;
};

class AstCondExpr: public AstExpr {
public:
    explicit AstCondExpr(csp::unique_ptr<AstExpr> cond, Token question,
                         csp::unique_ptr<AstExpr> ifExpr, Token colon,
                         csp::unique_ptr<AstExpr> thenExpr):
        AstExpr(AstNodeType::AstCondExpr, cond->firstToken(), std::move(cond),
                std::move(ifExpr), std::move(thenExpr)),
        question(question),
        colon(colon) {}

    AST_PROPERTY(0, AstExpr, condition, Condition)

    AST_PROPERTY(1, AstExpr, thenOperand, ThenOperand)

    AST_PROPERTY(2, AstExpr, elseOperand, elseOperand)

    Token questionToken() const { return question; }

    Token colonToken() const { return colon; }

private:
    Token question;
    Token colon;
};

class AstLogicalExpr: public AstBinaryExpr {
public:
    explicit AstLogicalExpr(Token opToken, AstLogicalOp op,
                            csp::unique_ptr<AstExpr> lhs,
                            csp::unique_ptr<AstExpr> rhs):
        AstBinaryExpr(AstNodeType::AstLogicalExpr, opToken, std::move(lhs),
                      std::move(rhs)),
        op(op) {}

    AstLogicalOp operation() const { return op; }

private:
    AstLogicalOp op;
};

class AstArithmeticExpr: public AstBinaryExpr {
public:
    explicit AstArithmeticExpr(Token opToken, AstArithmeticOp op,
                               csp::unique_ptr<AstExpr> lhs,
                               csp::unique_ptr<AstExpr> rhs):
        AstBinaryExpr(AstNodeType::AstArithmeticExpr, opToken, std::move(lhs),
                      std::move(rhs)),
        op(op) {}

    AstArithmeticOp operation() const { return op; }

private:
    AstArithmeticOp op;
};

class AstUnaryExpr: public AstExpr {
public:
    enum Kind { Prefix, Postfix };

    AstUnaryExpr(Kind kind, AstUnaryOp op, Token opToken,
                 csp::unique_ptr<AstExpr> operand):
        AstExpr(AstNodeType::AstUnaryExpr,
                /* first-token: */ kind == Prefix ? opToken :
                                                    operand->firstToken(),
                std::move(operand)),
        op(op) {}

    AST_PROPERTY(0, AstExpr, operand, Operand)

    AstUnaryOp operation() const { return op; }

private:
    AstUnaryOp op;
};

class AstCallBase: public AstExpr {
public:
    AST_PROPERTY(0, AstExpr, callee, Callee)

    AST_PROPERTY_RANGE(1, AstFacet, arguments, Callee)

protected:
    template <typename... Args>
    AstCallBase(AstNodeType nodeType, Token firstToken,
                csp::unique_ptr<AstExpr> callee, Args&&... arguments):
        AstExpr(nodeType, firstToken, std::move(callee),
                std::forward<Args>(arguments)...) {}
};

class AstCallExpr: public AstCallBase {
public:
    template <typename... Args>
    AstCallExpr(Token firstToken, csp::unique_ptr<AstExpr> callee,
                Args&&... arguments):
        AstCallBase(AstNodeType::AstCallExpr, firstToken, std::move(callee),
                    std::forward<Args>(arguments)...) {}
};

class AstConstructExpr: public AstCallBase {
public:
};

class AstAggregateExpr: public AstCallBase {
public:
};

class AstIndexExpr: public AstCallBase {
public:
};

class AstIndexSliceExpr: public AstCallBase {
public:
};

// MARK: - Statements

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
                          csp::unique_ptr<AstTypeSpec> typeSpec):
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

    AST_PROPERTY(2, AstTypeSpec, retTypeSpec, RetTypeSpec)

    AST_PROPERTY(3, AstCompoundStmt, body, Body)

    explicit AstFuncDecl(Token declarator, csp::unique_ptr<AstName> name,
                         csp::unique_ptr<AstParamList> params,
                         csp::unique_ptr<AstTypeSpec> retTypeSpec,
                         csp::unique_ptr<AstCompoundStmt> body):
        AstDecl(AstNodeType::AstFuncDecl, declarator, std::move(name),
                std::move(params), std::move(retTypeSpec), std::move(body)) {}
};

} // namespace prism

#undef AST_COMMON
#undef AST_PROPERTY
#undef AST_PROPERTY_RANGE

#endif // PRISM_AST_AST_H
