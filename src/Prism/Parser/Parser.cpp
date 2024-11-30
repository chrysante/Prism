// MARK: - Grammar
//
// clang-format off
//
// <source-file>        ::= <global-decl>*
// <global-decl>        ::= [<access-spec>] <decl>
// <decl>               ::= <func-decl> | <struct-decl> | <var-decl>
// <access-spec>        ::= 'public' | 'private'
//
// <func-decl>          ::= 'fn' <name> <param-list> ['->' <type-spec>] <compound-stmt>
// <param-list>         ::= '(' ')'
//                        | '(' [<param-decl> (',' <param-decl>)*] ')'
// <param-decl>         ::= <name> ':' <type-spec>
//
// <struct-decl>        ::= ('struct' | 'trait') <name> [':' <base-list>] <struct-body>
// <base-list>          ::= <base-decl> (',' <base-decl>)*
// <base-decl>          ::= [<access-spec>] <name>
// <struct-body>        ::= '{' <global-decl>* '}'
//
// <var-decl>           ::= ('var' | 'let') <name> [':' <type-spec>] ['=' <assign-expr>] ';'
//
// <stmt>               ::= <decl>
//                        | <ctrl-flow-stmt>
//                        | <compound-stmt>
//                        | <import-stmt>
//                        | <expr-stmt>
//                        | ';'
//
// <import-stmt>        ::= TODO
//
// <expr-stmt>          ::= <expr> ';'
//
// <compound-stmt>      ::= '{' <statement>* '}'
//
// <ctrl-flow-stmt>     ::= <return-stmt>
//                        | <if-stmt>
//                        | <while-stmt>
//                        | <do-while-stmt>
//                        | <for-stmt>
//
// <return-stmt>        ::= 'return' [<facet>] ';'
//
// <if-stmt>            ::= 'if' <comma-expr> <compound-stmt> ['else' (<if-stmt> | <compound-stmt>)]
//
// <while-stmt>         ::= 'while' <comma-expr> <compound-stmt>
//
// <do-while-stmt>      ::= 'do' <compound-stmt> 'while' <comma-expr> ';'
//
// <for-stmt>           ::= 'for' <var-decl> <comma-expr> ';' <comma-expr> <compound-stmt>
//
// <jump-stmt>          ::= ('break' | 'continue') ';'
//
// <expr>               ::= <facet>
// <assign-expr>        ::= <assign-facet>
// <type-spec>          ::= <prefix-facet>
// <facet>              ::= <comma-facet>
// <comma-facet>        ::= <assign-facet>
//                        | <comma-facet> ',' <assign-facet>
// <assign-facet>       ::= <cast-facet>
//                        | <cast-facet> ('=', '+=', ...) <assign-facet>
// <cast-facet>         ::= <cond-facet>
//                        | <cast-facet> 'as' <type-spec>
// <cond-facet>         ::= <logical-or-facet>
//                        | <logical-or-facet> '?' <comma-facet> ':' <cond-facet>
// <logical-or-facet>   ::= <logical-and-facet>
//                        | <logical-or-facet> '||' <logical-and-facet>
// <logical-and-facet>  ::= <or-facet>
//                        | <logical-and-facet> '&&' <or-facet>
// <or-facet>           ::= <xor-facet>
//                        | <or-facet> '|' <xor-facet>
// <xor-facet>          ::= <and-facet>
//                        | <xor-facet> '^' <and-facet>
// <and-facet>          ::= <eq-facet>
//                        | <and-facet> '^' <eq-facet>
// <eq-facet>           ::= <rel-facet>
//                        | <eq-facet> ('==' | '!=') <rel-facet>
// <rel-facet>          ::= <shift-facet>
//                        | <rel-facet> ('<' | '<=' | '>' | '>=')  <shift-facet>
// <shift-facet>        ::= <add-facet>
//                        | <shift-facet> ('<<' | '>>') <add-facet>
// <add-facet>          ::= <mul-facet>
//                        | <add-facet> ('+' | '-') <mul-facet>
// <mul-facet>          ::= <prefix-facet>
//                        | <mul-facet> ('*' | '/' | '%') <prefix-facet>
// <prefix-facet>       ::= <postfix-facet>
//                        | <prefix-op> <prefix-facet>
// <prefix-op>          ::= '+' | '-' | '~' | '!' | '++' | '--'
//                        | <ref-spec-facet>
//                        | '*' | '&' | '?'
//                        | 'new' ['unique' | 'shared']
//                        | 'move'
// <ref-spec-facet>     ::= 'mut' | 'dyn'
// <postfix-facet>      ::= <primary-facet>
//                        | <postfix-facet> ('++' | '--')
//                        | <postfix-facet> '(' ')'
//                        | <postfix-facet> '(' <arg-list> ')'
//                        | <postfix-facet> '{' <arg-list> '}'
//                        | <postfix-facet> '[' <assign-facet> ']'
//                        | <postfix-facet> '[' <assign-facet> (',' | ':') <assign-facet> ']'
// <arg-list>           ::= <assign-facet> (',' <assign-facet>)*
// <primary-facet>      ::= <identifier>
//                        | <integer-literal>
//                        | <boolean-literal>
//                        | <floating-point-literal>
//                        | <string-literal>
//                        | <fstring-facet>
//                        | 'this'
//                        | '(' <comma-facet> ')'
//                        | '[' <assign-facet>* ']'
// <fstring-facet>      ::= <fstring-begin> <assign-facet> <fstring-mid-facet>* <fstring-end>
// <fstring-mid-facet>  ::= <fstring-mid> <assign-facet>

// clang-format on

#include "Prism/Parser/Parser.h"

#include <concepts>
#include <cstdint>
#include <optional>

#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Lexer/Lexer.h"
#include "Prism/ParseTree/ParseTree.h"
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
    explicit Parser(SourceContext const& sourceCtx, ParseTreeContext& ptCtx,
                    IssueHandler& iss):
        sourceCtx(sourceCtx),
        ptCtx(ptCtx),
        lexer(sourceCtx.source(), iss),
        iss(iss) {}

    csp::unique_ptr<AstSourceFile> run();

    csp::unique_ptr<AstStmt> parseStmt();
    csp::unique_ptr<AstDecl> parseDecl();
    csp::unique_ptr<AstFuncDecl> parseFuncDecl();
    csp::unique_ptr<AstCompoundStmt> parseCompoundStmt();
    csp::unique_ptr<AstParamDecl> parseParamDecl();
    csp::unique_ptr<AstParamList> parseParamList();
    csp::unique_ptr<AstExprStmt> parseExprStmt();
    csp::unique_ptr<AstName> parseName();
    csp::unique_ptr<AstUnqualName> parseUnqualName();
    csp::unique_ptr<AstFacet> parseFacet();
    csp::unique_ptr<AstExpr> parseExpr();
    csp::unique_ptr<AstTypeSpec> parseTypeSpec();
    template <typename Abstract, typename Concrete>
    csp::unique_ptr<Abstract> parseFacetImpl(ParserFn auto start);

    // MARK: - Facets
    ParseTreeNode const* parseCommaFacet();
    ParseTreeNode const* parseAssignFacet();
    ParseTreeNode const* parseCastFacet();
    ParseTreeNode const* parseCondFacet();
    ParseTreeNode const* parseLogicalOrFacet();
    ParseTreeNode const* parseLogicalAndFacet();
    ParseTreeNode const* parseOrFacet();
    ParseTreeNode const* parseXorFacet();
    ParseTreeNode const* parseAndFacet();
    ParseTreeNode const* parseEqFacet();
    ParseTreeNode const* parseRelFacet();
    ParseTreeNode const* parseShiftFacet();
    ParseTreeNode const* parseAddFacet();
    ParseTreeNode const* parseMulFacet();
    ParseTreeNode const* parsePrefixFacet();
    ParseTreeNode const* parsePostfixFacet();
    ParseTreeNode const* parsePrimaryFacet();
    ParseTreeNode const* parseFstringFacet();

    ParseTreeNode const* parseBinaryFacetLTR(
        std::span<TokenKind const> acceptedOperators, ParserFn auto next);
    ParseTreeNode const* parseBinaryFacetRTL(
        std::span<TokenKind const> acceptedOperators, ParserFn auto next);

    template <ParserFn Fn>
    utl::small_vector<InvokeResult<Fn>> parseSequence(
        Fn parser, TokenKind end,
        std::optional<TokenKind> delim = std::nullopt);

    /// _Eats_ and returns the eaten token, if its kind is any of the given
    /// arguments. Otherwise returns nullopt
    std::optional<Token> match(TokenKind tok,
                               std::same_as<TokenKind> auto... rest);

    /// \overload
    std::optional<Token> match(std::span<TokenKind const> tokenKinds);

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
    ParseTreeContext& ptCtx;
    IssueHandler& iss;
    Lexer lexer;
    std::optional<Token> peekToken;
    bool inRecovery = false;
};

} // namespace

csp::unique_ptr<AstSourceFile> prism::parseSourceFile(
    SourceContext const& sourceCtx, ParseTreeContext& ptCtx,
    IssueHandler& iss) {
    return Parser(sourceCtx, ptCtx, iss).run();
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
    auto retType = match(Arrow) ? parseTypeSpec() : nullptr;
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
    auto typeSpec = colon ? parseTypeSpec() :
                            recover(Colon, Comma, &Parser::parseTypeSpec);
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

csp::unique_ptr<AstFacet> Parser::parseFacet() {
    return parseFacetImpl<AstFacet, AstFacetPlaceholder>(
        &Parser::parseCommaFacet);
}

csp::unique_ptr<AstExpr> Parser::parseExpr() {
    return parseFacetImpl<AstExpr, AstExprPlaceholder>(
        &Parser::parseCommaFacet);
}

csp::unique_ptr<AstTypeSpec> Parser::parseTypeSpec() {
    return parseFacetImpl<AstTypeSpec, AstTypeSpecPlaceholder>(
        &Parser::parsePrefixFacet);
}

static Token firstToken(ParseTreeNode const* node) {
    PRISM_ASSERT(node);
    // clang-format off
    csp::visit(*node, csp::overload{
        [](ParseTreeTerminal const& node) {
            return node.token();
        },
        [](ParseTreeNonTerminal const& node) {
            return firstToken(node.childAt(0));
        }
    });
    // clang-format on
}

template <typename Abstract, typename Concrete>
csp::unique_ptr<Abstract> Parser::parseFacetImpl(ParserFn auto start) {
    auto* facet = invoke(start);
    if (!facet) return nullptr;
    return csp::make_unique<Concrete>(facet, firstToken(facet));
}

// MARK: - Facets
ParseTreeNode const* Parser::parseCommaFacet() {
    return parseBinaryFacetLTR({ { Comma } }, &Parser::parseAssignFacet);
}

ParseTreeNode const* Parser::parseAssignFacet() {
    return parseBinaryFacetRTL({ { Equal, PlusEq, MinusEq, StarEq, SlashEq,
                                   PercentEq, AmpersandEq, VertBarEq,
                                   CircumflexEq } },
                               &Parser::parseCastFacet);
}

ParseTreeNode const* Parser::parseCastFacet() {
    return parseCondFacet(); // For now
}

ParseTreeNode const* Parser::parseCondFacet() {
    auto* cond = parseLogicalOrFacet();
    if (!cond) return nullptr;
    auto question = match(Question);
    if (!question) return cond;
    auto lhs = parseCommaFacet();
    if (!lhs) {
        assert(false); // Expected expression
    }
    auto colon = match(Colon);
    if (!colon) {
        assert(false); // Expected colon
    }
    auto* rhs = parseCondFacet();
    if (!rhs) {
        assert(false); // Expected expression
    }
    return ptCtx.condFacet(cond, *question, lhs, *colon, rhs);
}

ParseTreeNode const* Parser::parseLogicalOrFacet() {
    return parseBinaryFacetLTR({ { DoubleVertBar } },
                               &Parser::parseLogicalAndFacet);
}

ParseTreeNode const* Parser::parseLogicalAndFacet() {
    return parseBinaryFacetLTR({ { DoubleAmpersand } }, &Parser::parseOrFacet);
}

ParseTreeNode const* Parser::parseOrFacet() {
    return parseBinaryFacetLTR({ { VertBar } }, &Parser::parseXorFacet);
}

ParseTreeNode const* Parser::parseXorFacet() {
    return parseBinaryFacetLTR({ { Circumflex } }, &Parser::parseAndFacet);
}

ParseTreeNode const* Parser::parseAndFacet() {
    return parseBinaryFacetLTR({ { Ampersand } }, &Parser::parseEqFacet);
}

ParseTreeNode const* Parser::parseEqFacet() {
    return parseBinaryFacetLTR({ { Equal, NotEq } }, &Parser::parseRelFacet);
}

ParseTreeNode const* Parser::parseRelFacet() {
    return parseBinaryFacetLTR({ { LeftAngle, LeftAngleEq, RightAngle,
                                   RightAngleEq } },
                               &Parser::parseShiftFacet);
}

ParseTreeNode const* Parser::parseShiftFacet() {
    return parseBinaryFacetLTR({ { DoubleLeftAngle, DoubleRightAngle } },
                               &Parser::parseAddFacet);
}

ParseTreeNode const* Parser::parseAddFacet() {
    return parseBinaryFacetLTR({ { Plus, Minus } }, &Parser::parseMulFacet);
}

ParseTreeNode const* Parser::parseMulFacet() {
    return parseBinaryFacetLTR({ { Star, Slash, Percent } },
                               &Parser::parsePrefixFacet);
}

ParseTreeNode const* Parser::parsePrefixFacet() {
    return parsePostfixFacet(); // For now
}

ParseTreeNode const* Parser::parsePostfixFacet() {
    return parsePrimaryFacet(); // For now
}

ParseTreeNode const* Parser::parsePrimaryFacet() {
    if (auto tok = match(Identifier)) {
        return ptCtx.terminal(*tok);
    }
    return nullptr;
}

ParseTreeNode const* Parser::parseFstringFacet() { return nullptr; }

ParseTreeNode const* Parser::parseBinaryFacetLTR(
    std::span<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = invoke(next);
    if (!lhs) return nullptr;
    while (true) {
        auto tok = match(acceptedOperators);
        if (!tok) return lhs;
        auto* rhs = invoke(next);
        if (!rhs) {
            assert(false); // Expected facet
        }
        lhs = ptCtx.binaryFacet(lhs, *tok, rhs);
    }
}

ParseTreeNode const* Parser::parseBinaryFacetRTL(
    std::span<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = invoke(next);
    if (!lhs) return nullptr;
    if (auto tok = match(acceptedOperators)) {
        auto* rhs = parseBinaryFacetRTL(acceptedOperators, next);
        if (!rhs) {
            assert(false); // Expected facet
        }
        return ptCtx.binaryFacet(lhs, *tok, rhs);
    }
    return lhs;
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

std::optional<Token> Parser::match(std::span<TokenKind const> tokenKinds) {
    auto tok = peek();
    if (ranges::contains(tokenKinds, tok.kind)) {
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
