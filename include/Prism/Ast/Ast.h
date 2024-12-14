#ifndef PRISM_AST_AST_H
#define PRISM_AST_AST_H

#include <bit>
#include <optional>
#include <span>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include <Prism/Ast/AstFwd.h>
#include <Prism/Ast/Operators.h>
#include <Prism/Common/Allocator.h>
#include <Prism/Common/Functional.h>
#include <Prism/Source/Token.h>

#define AST_COMMON(NodeName) friend class AstNode;

#define AST_PROPERTY(Index, Type, Name, CapName)                               \
    Type* Name() { return childAt<Type>(Index); }                              \
    Type const* Name() const { return childAt<Type>(Index); }

#define AST_PROPERTY_RANGE(BeginIndex, Type, Name, CapName)                    \
    std::span<Type* const> Name##s() { return children<Type>(BeginIndex); }    \
    std::span<Type const* const> Name##s() const {                             \
        return children<Type>(BeginIndex);                                     \
    }

namespace prism {

class SourceContext;
class Facet;

// MARK: - Base nodes

/// Base class of all AST nodes
class AstNode: public csp::base_helper<AstNode> {
public:
    Token firstToken() const { return firstTok; }

    AstNode* childAt(size_t index) { return _children[index]; }

    AstNode const* childAt(size_t index) const { return _children[index]; }

    template <std::derived_from<AstNode> T>
    T* childAt(size_t index) {
        return csp::cast<T*>(childAt(index));
    }

    template <std::derived_from<AstNode> T>
    T const* childAt(size_t index) const {
        return csp::cast<T const*>(childAt(index));
    }

    std::span<AstNode* const> children() { return _children; }

    std::span<AstNode const* const> children() const { return _children; }

    template <std::derived_from<AstNode> C>
    std::span<C* const> children(size_t drop = 0) {
        auto c = std::as_const(*this).children<C>(drop);
        return { const_cast<C* const*>(c.data()), c.size() };
    }

    template <std::derived_from<AstNode> C>
    std::span<C const* const> children(size_t drop = 0) const {
        auto c = children().subspan(drop);
        PRISM_ASSERT(ranges::all_of(c, csp::isa<C>));
        return { std::bit_cast<C const* const*>(c.data()), c.size() };
    }

protected:
    template <typename T>
    using Vector =
        utl::vector<T, ResourceAllocator<T, MonotonicBufferResource>>;

    template <typename... C>
    explicit AstNode(AstNodeType type, MonotonicBufferResource* res,
                     Token firstToken, C&&... children):
        base_helper(type),
        firstTok(firstToken),
        _children(toVec(res, std::forward<C>(children)...)) {}

private:
    template <typename T>
    static size_t vecSize(T const& child) {
        if constexpr (ranges::range<T>) {
            return ranges::size(child);
        }
        else {
            return 1;
        }
    }

    static void insertChildren(Vector<AstNode*>& vec, auto* child) {
        vec.push_back(child);
    }

    static void insertChildren(Vector<AstNode*>& vec,
                               ranges::range auto&& children) {
        ranges::for_each(children, [&](auto&& elem) {
            insertChildren(vec, std::move(elem));
        });
    }

    static Vector<AstNode*> toVec(MonotonicBufferResource* res) {
        return Vector<AstNode*>(nullptr);
    }

    template <typename... T>
    static Vector<AstNode*> toVec(MonotonicBufferResource* res,
                                  T&&... children) {
        Vector<AstNode*> result(res);
        result.reserve((0 + ... + vecSize(children)));
        (insertChildren(result, std::forward<T>(children)), ...);
        return { std::move(result), res };
    }

    Token firstTok;
    Vector<AstNode*> _children;
};

/// Base class of all AST statements
class AstStmt: public AstNode {
protected:
    using AstNode::AstNode;
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
    explicit AstRawFacet(MonotonicBufferResource* res, Facet const* fct,
                         Token firstTok):
        AstFacet(AstNodeType::AstRawFacet, res, firstTok), RawFacetBase(fct) {}
};

// MARK: - Type Specifier

/// Type specifier. This is populated with a type facet during parsing and sema
/// will directly assign the analyzed type symbol
class AstTypeSpec: public AstFacet, public RawFacetBase {
public:
    explicit AstTypeSpec(MonotonicBufferResource* res, Facet const* fct,
                         Token firstTok):
        AstFacet(AstNodeType::AstTypeSpec, res, firstTok), RawFacetBase(fct) {}
};

// MARK: - Base Expressions

/// Abstract base class of AST expressions
class AstExpr: public AstFacet {
protected:
    using AstFacet::AstFacet;
};

///
class AstFacetExpr: public AstExpr, public RawFacetBase {
public:
    explicit AstFacetExpr(MonotonicBufferResource* res, Facet const* fct,
                          Token firstTok):
        AstExpr(AstNodeType::AstFacetExpr, res, firstTok), RawFacetBase(fct) {}
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
        AstName(AstNodeType::AstUnqualName, nullptr, name) {}

    Token nameToken() const { return firstToken(); }
};

/// Qualified name, consisting of a sequence of tokens separated by `.`
class AstNameSeq: public AstName {
public:
    explicit AstNameSeq(MonotonicBufferResource* res,
                        std::span<Token const> seq):
        AstName(AstNodeType::AstNameSeq, res, seq.front()),
        _seq(seq.begin(), seq.end(), res) {}

    std::span<Token const> tokenSeq() const { return _seq; }

private:
    Vector<Token> _seq;
};

///
class AstCompoundExpr: public AstExpr {
public:
    explicit AstCompoundExpr(MonotonicBufferResource* res, Token openBrace,
                             std::span<AstStmt* const> stmts):
        AstExpr(AstNodeType::AstCompoundExpr, res, openBrace, stmts) {}

    AST_PROPERTY_RANGE(0, AstStmt, statement, Statement)
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
    explicit AstDecl(AstNodeType type, MonotonicBufferResource* res,
                     Token declarator, AstName* name, C&&... otherChildren):
        AstStmt(type, res, declarator, name,
                std::forward<C>(otherChildren)...) {}
};

/// Base class of all declaration introducing a variable, i.e. variable
/// declarations and parameter declarations
class AstVarDeclBase: public AstDecl {
public:
    AST_PROPERTY(1, AstTypeSpec, typeSpec, TypeSpec)

    std::optional<Token> colon() const { return _colon; }

protected:
    explicit AstVarDeclBase(AstNodeType astType, MonotonicBufferResource* res,
                            Token firstToken, AstName* name,
                            std::optional<Token> colon, AstTypeSpec* typeSpec,
                            auto*... args):
        AstDecl(astType, res, firstToken, name, typeSpec, args...),
        _colon(colon) {}

private:
    std::optional<Token> _colon;
};

///
class AstParamDecl: public AstVarDeclBase {
public:
    explicit AstParamDecl(MonotonicBufferResource* res, AstUnqualName* name,
                          Token colon, AstTypeSpec* typeSpec):
        AstVarDeclBase(AstNodeType::AstParamDecl, res, name->firstToken(), name,
                       colon, typeSpec) {}

    Token colon() const { return AstVarDeclBase::colon().value(); }
};

///
class AstVarDecl: public AstVarDeclBase {
public:
    explicit AstVarDecl(MonotonicBufferResource* res, Token declarator,
                        AstName* name, std::optional<Token> colon,
                        AstTypeSpec* typeSpec, std::optional<Token> eq,
                        AstExpr* initExpr):
        AstVarDeclBase(AstNodeType::AstVarDecl, res, declarator, name, colon,
                       typeSpec, initExpr),
        eq(eq) {}

    AST_PROPERTY(2, AstExpr, initExpr, InitExpr)

    std::optional<Token> eqToken() const { return eq; }

private:
    std::optional<Token> eq;
};

/// Comma separated sequence of parameter declarations
class AstParamList: public AstNode {
public:
    explicit AstParamList(MonotonicBufferResource* res, Token openParen,
                          Token closeParen,
                          std::span<AstParamDecl* const> params):
        AstNode(AstNodeType::AstParamList, res, openParen, params),
        _closeParen(closeParen) {}

    AST_PROPERTY_RANGE(0, AstParamDecl, param, Param)

    Token openParen() const { return firstToken(); }

    Token closeParen() const { return _closeParen; }

private:
    Token _closeParen;
};

/// Closure expressions. This will be rewritten by sema, so we never generate
/// code for these directly
class AstClosureExpr: public AstExpr {
public:
    explicit AstClosureExpr(MonotonicBufferResource* res, Token fn,
                            AstParamList* params, AstTypeSpec* retType,
                            AstExpr* body):
        AstExpr(AstNodeType::AstClosureExpr, res, fn, params, retType, body) {}

    AST_PROPERTY(0, AstParamList, params, Params)

    AST_PROPERTY(1, AstTypeSpec, retTypeSpec, RetTypeSpec)

    AST_PROPERTY(2, AstExpr, body, Body)
};

// MARK: - Statements

/// List of declarations in a source file
class AstSourceFile: public AstNode {
public:
    AST_COMMON(AstSourceFile)

    /// The top level declarations
    AST_PROPERTY_RANGE(0, AstDecl, decl, Decl)

    explicit AstSourceFile(MonotonicBufferResource* res,
                           SourceContext const& ctx,
                           std::span<AstDecl* const> decls):
        AstNode(AstNodeType::AstSourceFile, res, Token::ErrorToken, decls),
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

    explicit AstTranslationUnit(MonotonicBufferResource* res,
                                std::span<AstSourceFile* const> sourceFiles):
        AstNode(AstNodeType::AstTranslationUnit, res, Token::ErrorToken,
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
    explicit AstBinaryExpr(AstNodeType nodeType, MonotonicBufferResource* res,
                           Token opToken, AstExpr* lhs, AstExpr* rhs):
        AstExpr(nodeType, res, lhs->firstToken(), lhs, rhs), opTok(opToken) {}

private:
    Token opTok;
};

class AstCommaExpr: public AstBinaryExpr {
public:
    explicit AstCommaExpr(MonotonicBufferResource* res, Token commaToken,
                          AstExpr* lhs, AstExpr* rhs):
        AstBinaryExpr(AstNodeType::AstCommaExpr, res, commaToken, lhs, rhs) {}
};

class AstAssignExpr: public AstBinaryExpr {
public:
    explicit AstAssignExpr(MonotonicBufferResource* res, Token opToken,
                           AstExpr* lhs, AstExpr* rhs):
        AstBinaryExpr(AstNodeType::AstAssignExpr, res, opToken, lhs, rhs) {}
};

class AstCastExpr: public AstExpr {
public:
    explicit AstCastExpr(MonotonicBufferResource* res, Token opToken,
                         AstExpr* operand, AstTypeSpec* targetType):
        AstExpr(AstNodeType::AstCastExpr, res, operand->firstToken(), operand,
                targetType),
        opTok(opToken) {}

    AST_PROPERTY(0, AstExpr, operand, Operand)

    AST_PROPERTY(1, AstTypeSpec, targetType, TargetType)

    Token opToken() const { return opTok; }

private:
    Token opTok;
};

class AstCondExpr: public AstExpr {
public:
    explicit AstCondExpr(MonotonicBufferResource* res, AstExpr* cond,
                         Token question, AstExpr* ifExpr, Token colon,
                         AstExpr* thenExpr):
        AstExpr(AstNodeType::AstCondExpr, res, cond->firstToken(), cond, ifExpr,
                thenExpr),
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
    explicit AstLogicalExpr(MonotonicBufferResource* res, Token opToken,
                            AstLogicalOp op, AstExpr* lhs, AstExpr* rhs):
        AstBinaryExpr(AstNodeType::AstLogicalExpr, res, opToken, lhs, rhs),
        op(op) {}

    AstLogicalOp operation() const { return op; }

private:
    AstLogicalOp op;
};

class AstArithmeticExpr: public AstBinaryExpr {
public:
    explicit AstArithmeticExpr(MonotonicBufferResource* res, Token opToken,
                               AstArithmeticOp op, AstExpr* lhs, AstExpr* rhs):
        AstBinaryExpr(AstNodeType::AstArithmeticExpr, res, opToken, lhs, rhs),
        op(op) {}

    AstArithmeticOp operation() const { return op; }

private:
    AstArithmeticOp op;
};

class AstUnaryExpr: public AstExpr {
public:
    enum Kind { Prefix, Postfix };

    AstUnaryExpr(MonotonicBufferResource* res, Kind kind, AstUnaryOp op,
                 Token opToken, AstExpr* operand):
        AstExpr(AstNodeType::AstUnaryExpr, res,
                /* first-token: */ kind == Prefix ? opToken :
                                                    operand->firstToken(),
                operand),
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
    AstCallBase(AstNodeType nodeType, MonotonicBufferResource* res,
                Token firstToken, AstExpr* callee, Args&&... arguments):
        AstExpr(nodeType, res, firstToken, callee,
                std::forward<Args>(arguments)...) {}
};

class AstCallExpr: public AstCallBase {
public:
    template <typename... Args>
    AstCallExpr(MonotonicBufferResource* res, Token firstToken, AstExpr* callee,
                Args&&... arguments):
        AstCallBase(AstNodeType::AstCallExpr, res, firstToken, callee,
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
    explicit AstExprStmt(MonotonicBufferResource* res, AstExpr* expr):
        AstStmt(AstNodeType::AstExprStmt, res, expr->firstToken(), expr) {}

    /// The wrapped expression
    AST_PROPERTY(0, AstExpr, expr, Expr)
};

/// A yield statement is a "statement-wrapper" around the terminating expression
/// in a compound expression
class AstYieldStmt: public AstStmt {
public:
    explicit AstYieldStmt(MonotonicBufferResource* res, AstExpr* expr):
        AstStmt(AstNodeType::AstYieldStmt, res, expr->firstToken(), expr) {}
};

///
class AstEmptyStmt: public AstStmt {
public:
    explicit AstEmptyStmt(MonotonicBufferResource* res, Token semicolon):
        AstStmt(AstNodeType::AstEmptyStmt, res, semicolon) {}
};

/// Function declaration or definition
class AstFuncDecl: public AstDecl {
public:
    AST_COMMON(AstFuncDecl);

    AST_PROPERTY(1, AstParamList, params, Params)

    AST_PROPERTY(2, AstTypeSpec, retTypeSpec, RetTypeSpec)

    AST_PROPERTY(3, AstCompoundExpr, body, Body)

    explicit AstFuncDecl(MonotonicBufferResource* res, Token declarator,
                         AstName* name, AstParamList* params,
                         AstTypeSpec* retTypeSpec, AstCompoundExpr* body):
        AstDecl(AstNodeType::AstFuncDecl, res, declarator, name, params,
                retTypeSpec, body) {}
};

} // namespace prism

#undef AST_COMMON
#undef AST_PROPERTY
#undef AST_PROPERTY_RANGE

#endif // PRISM_AST_AST_H
