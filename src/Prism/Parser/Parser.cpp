// --- GRAMMAR -------------------------------------------------------------- //
//
// clang-format off
//
// <source-file>    ::= <global-decl>*
// <global-decl>    ::= <func-decl> | <var-decl> | <struct-decl>
// <func-decl>      ::= 'fn' <name> <param-list> ['->' <type-expr>] <compound-stmt>
// <param-list>     ::= '(' [<param-decl> [',' <param-decl>]*] ')'
// <param-decl>     ::= <name> ':' <type-expr>
// <expr>           ::= <assign-expr>
// <name>           ::= <id-token>
//
// clang-format on

#include "Prism/Parser/Parser.h"

#include <concepts>
#include <cstdint>
#include <optional>

#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Lexer/Lexer.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using enum TokenKind;

namespace {

struct Parser;

template <typename Fn>
concept ParserFn = std::invocable<Fn> || std::invocable<Fn, Parser>;

template <ParserFn Fn>
using InvokeResult =
    typename std::conditional_t<std::invocable<Fn>, std::invoke_result<Fn>,
                                std::invoke_result<Fn, Parser>>::type;

struct Parser {
    explicit Parser(SourceContext const& sourceCtx, IssueHandler& iss):
        sourceCtx(sourceCtx), lexer(sourceCtx.source(), iss), iss(iss) {}

    csp::unique_ptr<AstSourceFile> run();

    csp::unique_ptr<AstStmt> parseStmt();
    csp::unique_ptr<AstDecl> parseDecl();
    csp::unique_ptr<AstFuncDecl> parseFuncDecl();
    csp::unique_ptr<AstCompoundStmt> parseCompoundStmt();
    csp::unique_ptr<AstParamDecl> parseParamDecl();
    csp::unique_ptr<AstParamList> parseParamList();
    csp::unique_ptr<AstExprStmt> parseExprStmt();
    csp::unique_ptr<AstExpr> parseExpr();
    csp::unique_ptr<AstExpr> parseArithmetic();
    csp::unique_ptr<AstName> parseName();
    csp::unique_ptr<AstUnqualName> parseUnqualName();

    template <ParserFn Fn>
    utl::small_vector<InvokeResult<Fn>> parseSequence(
        Fn parser, TokenKind end,
        std::optional<TokenKind> delim = std::nullopt);

    /// _Eats_ and returns the eaten token, if its kind is any of the given
    /// arguments. Otherwise returns nullopt
    std::optional<Token> match(TokenKind tok,
                               std::same_as<TokenKind> auto... rest);

    /// _Peeks_ and returns the peeked token, if its kind is any of the given
    /// arguments. Otherwise returns nullopt
    std::optional<Token> peekMatch(TokenKind tok,
                                   std::same_as<TokenKind> auto... rest);

    /// \Returns the next token in the stream without consuming it
    Token peek();

    /// \Returns the next token in the stream and consumes it
    Token eat();

    template <ParserFn Fn>
    InvokeResult<Fn> recover(TokenKind missing, std::optional<TokenKind> stop,
                             Fn next);

    template <ParserFn Fn>
    InvokeResult<Fn> invoke(Fn fn);

    SourceContext const& sourceCtx;
    std::optional<Token> peekToken;
    Lexer lexer;
    IssueHandler& iss;
    bool inRecovery = false;
};

} // namespace

csp::unique_ptr<AstSourceFile> prism::parseSourceFile(
    SourceContext const& sourceCtx, IssueHandler& iss) {
    return Parser(sourceCtx, iss).run();
}

csp::unique_ptr<AstSourceFile> Parser::run() {
    return csp::make_unique<AstSourceFile>(sourceCtx,
                                           parseSequence(&Parser::parseDecl,
                                                         End));
}

csp::unique_ptr<AstStmt> Parser::parseStmt() {
    if (auto decl = parseDecl()) {
        return decl;
    }
    if (auto stmt = parseExprStmt()) {
        return stmt;
    }
    return nullptr;
}

csp::unique_ptr<AstDecl> Parser::parseDecl() {
    if (auto function = parseFuncDecl()) {
        return function;
    }
    return nullptr;
}

csp::unique_ptr<AstFuncDecl> Parser::parseFuncDecl() {
    auto declarator = match(Function);
    if (!declarator) return nullptr;
    auto name = parseName();
    auto params = parseParamList();
    auto retType = match(Arrow) ? parseExpr() : nullptr;
    auto body = parseCompoundStmt();
    return csp::make_unique<AstFuncDecl>(*declarator, std::move(name),
                                         std::move(params), std::move(retType),
                                         std::move(body));
}

csp::unique_ptr<AstCompoundStmt> Parser::parseCompoundStmt() {
    auto openBrace = match(OpenBrace);
    if (!openBrace) return nullptr;
    auto statements = parseSequence(&Parser::parseStmt, CloseBrace);
    auto closeBrace = match(CloseBrace).value_or(Token::ErrorToken);
    return csp::make_unique<AstCompoundStmt>(*openBrace, closeBrace,
                                             std::move(statements));
}

csp::unique_ptr<AstParamDecl> Parser::parseParamDecl() {
    auto name = parseUnqualName();
    auto colon = match(Colon);
    auto typeSpec = colon ? parseExpr() :
                            recover(Colon, Comma, &Parser::parseExpr);
    return csp::make_unique<AstParamDecl>(std::move(name),
                                          colon.value_or(Token::ErrorToken),
                                          std::move(typeSpec));
}

csp::unique_ptr<AstParamList> Parser::parseParamList() {
    auto openParen = match(OpenParen);
    if (!openParen) return nullptr;
    auto seq = parseSequence(&Parser::parseParamDecl, CloseParen, Comma);
    auto closeParen = match(CloseParen).value_or(Token::ErrorToken);
    return csp::make_unique<AstParamList>(*openParen, closeParen,
                                          std::move(seq));
}

csp::unique_ptr<AstExprStmt> Parser::parseExprStmt() {
    auto expr = parseExpr();
    if (!expr) return nullptr;
    if (!match(Semicolon)) {
        assert(false); // Push error
    }
    return csp::make_unique<AstExprStmt>(std::move(expr));
}

csp::unique_ptr<AstExpr> Parser::parseExpr() { return parseArithmetic(); }

static std::optional<AstArithmeticOp> toArithmeticOp(TokenKind tok) {
    switch (tok) {
#define AST_ARITHMETIC_OPERATOR(Name, AssocToken)                              \
    case AssocToken:                                                           \
        return AstArithmeticOp::Name;
#include "Prism/Ast/Ast.def"
    default:
        return std::nullopt;
    }
}

csp::unique_ptr<AstExpr> Parser::parseArithmetic() {
    csp::unique_ptr<AstExpr> lhs = parseName();
    if (!lhs) return nullptr;
    while (true) {
        Token opTok = peek();
        auto op = toArithmeticOp(opTok.kind);
        if (!op) {
            return lhs;
        }
        eat();
        auto rhs = parseName();
        if (!rhs) {
            assert(false); // Expected expression
        }
        lhs = csp::make_unique<AstArithmeticExpr>(*op, opTok, std::move(lhs),
                                                  std::move(rhs));
    }
}

csp::unique_ptr<AstName> Parser::parseName() { return parseUnqualName(); }

csp::unique_ptr<AstUnqualName> Parser::parseUnqualName() {
    if (auto tok = match(Identifier)) {
        return csp::make_unique<AstUnqualName>(*tok);
    }
    return nullptr;
}

template <ParserFn Fn>
utl::small_vector<InvokeResult<Fn>> Parser::parseSequence(
    Fn parser, TokenKind end, std::optional<TokenKind> delim) {
    utl::small_vector<InvokeResult<Fn>> seq;
    bool first = true;
    while (true) {
        if (peekMatch(end)) {
            return seq;
        }
        if (!first && delim && !match(*delim)) {
            assert(false); // Push error here
        }
        first = false;
        seq.push_back(invoke(parser));
    }
}

std::optional<Token> Parser::match(TokenKind kind,
                                   std::same_as<TokenKind> auto... rest) {
    auto tok = peek();
    if (tok.kind == kind || (... || (tok.kind == rest))) {
        return eat();
    }
    return std::nullopt;
}

std::optional<Token> Parser::peekMatch(TokenKind kind,
                                       std::same_as<TokenKind> auto... rest) {

    auto tok = peek();
    if (tok.kind == kind || (... || (tok.kind == rest))) {
        return tok;
    }
    return std::nullopt;
}

Token Parser::peek() {
    if (peekToken) {
        return *peekToken;
    }
    auto tok = lexer.next();
    peekToken = tok;
    return tok;
}

Token Parser::eat() {
    if (peekToken) {
        auto tok = *peekToken;
        peekToken.reset();
        return tok;
    }
    return lexer.next();
}

template <ParserFn Fn>
InvokeResult<Fn> Parser::recover(TokenKind missing,
                                 std::optional<TokenKind> stop, Fn next) {
    if (inRecovery) {
        return nullptr;
    }
    inRecovery = true;
    utl::scope_guard reset = [this] { inRecovery = false; };
    static size_t const NumAttempts = 3;
    for (size_t i = 0; i < NumAttempts; ++i) {
        if (stop && peekMatch(*stop)) {
            return nullptr;
        }
        if (auto result = invoke(next)) {
            return result;
        }
        eat();
    }
    return nullptr;
}

template <ParserFn Fn>
InvokeResult<Fn> Parser::invoke(Fn fn) {
    if constexpr (std::invocable<Fn>) {
        return std::invoke(fn);
    }
    else {
        return std::invoke(fn, *this);
    }
}
