#include "Prism/Parser/Parser.h"

#include <concepts>
#include <cstdint>
#include <optional>

#include <utl/function_view.hpp>
#include <utl/scope_guard.hpp>

#include "Prism/Ast/Ast.h"
#include "Prism/Ast/Facet.h"
#include "Prism/Common/Assert.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Lexer/Lexer.h"
#include "Prism/Parser/SyntaxIssue.h"
#include "Prism/Source/SourceContext.h"

#include "Prism/Common/SyntaxMacros.h"

using namespace prism;

using enum TokenKind;

static constexpr Token ErrorToken = Token::ErrorToken;

namespace {

struct Parser;

template <typename Fn>
concept ParserFn = std::invocable<Fn> || std::invocable<Fn, Parser>;

template <ParserFn Fn>
using InvokeResult =
    typename std::conditional_t<std::invocable<Fn>, std::invoke_result<Fn>,
                                std::invoke_result<Fn, Parser>>::type;

enum class FacetState { General, Type };

template <typename T>
struct VolatileList: std::span<T> {
    template <typename... Args>
        requires std::constructible_from<std::span<T>, Args&&...>
    VolatileList(Args&&... args): std::span<T>(std::forward<Args>(args)...) {}
    VolatileList(std::span<T> s): std::span<T>(s) {}
    VolatileList(std::initializer_list<std::remove_const_t<T>> ilist):
        std::span<T>(ilist) {}
    VolatileList(T& kind): std::span<T>(&kind, 1) {}
};

struct RecoveryOptions {
    VolatileList<TokenKind const> stop;
    int numAttempts = 3;
};

struct Parser {
    explicit Parser(MonotonicBufferResource& alloc,
                    SourceContext const& sourceCtx, IssueHandler& iss):
        alloc(alloc),
        sourceCtx(sourceCtx),
        lexer(sourceCtx.source(), iss),
        iss(iss) {}

    AstSourceFile* parseSourceFile();
    AstStmt* parseStmt();
    AstDecl* parseDecl();
    AstFuncDecl* parseFuncDecl();
    AstCompoundStmt* parseCompoundStmt();
    AstParamDecl* parseParamDecl();
    AstParamList* parseParamList();
    AstExprStmt* parseExprStmt();
    AstName* parseName();
    AstUnqualName* parseUnqualName();
    AstFacet* parseFacet();
    AstExpr* parseExpr();
    AstTypeSpec* parseTypeSpec();
    template <typename Abstract, typename Concrete>
    Abstract* parseFacetImpl(ParserFn auto start);

    // MARK: - Facets
    Facet const* parseCommaFacet();
    Facet const* parseAssignFacet();
    Facet const* parseCastFacet();
    Facet const* parseTernCondFacet();
    Facet const* parseBinCondFacet();
    Facet const* parseLogicalOrFacet();
    Facet const* parseLogicalAndFacet();
    Facet const* parseOrFacet();
    Facet const* parseXorFacet();
    Facet const* parseAndFacet();
    Facet const* parseEqFacet();
    Facet const* parseRelFacet();
    Facet const* parseShiftFacet();
    Facet const* parseAddFacet();
    Facet const* parseMulFacet();
    Facet const* parsePrefixFacet();
    Facet const* parsePostfixFacet();
    Facet const* parsePrimaryFacet();
    Facet const* parseFstringFacet();

    Facet const* parseBinaryFacetLTR(
        VolatileList<TokenKind const> acceptedOperators, ParserFn auto next);
    Facet const* parseBinaryFacetRTL(
        VolatileList<TokenKind const> acceptedOperators, ParserFn auto next);

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

    ///
    template <ParserFn Fn>
    InvokeResult<Fn> recover(Fn next, RecoveryOptions options);

    ///
    template <ParserFn Fn>
    bool validOrRecover(InvokeResult<Fn>& obj, Fn next,
                        RecoveryOptions options);

    ///
    template <ParserFn Fn>
    InvokeResult<Fn> parseOrRecover(Fn next, RecoveryOptions options);

    template <ParserFn Fn>
    InvokeResult<Fn> parseOrRecover(Fn next, RecoveryOptions options,
                                    ParserFn auto raiseIssue);

    template <std::derived_from<Issue> I, typename... Args>
        requires std::constructible_from<I, Token, Args&&...>
    auto raiseCb(Args&&... args) {
        return [this, ... args = std::forward<Args>(args)] {
            raise<I>(peek(), args...);
        };
    }

    template <std::derived_from<Issue> I, typename... Args>
        requires std::constructible_from<I, Args&&...>
    void raise(Args&&... args) {
        iss.push<I>(std::forward<Args>(args)...);
    }

    template <ParserFn Fn>
    InvokeResult<Fn> invoke(Fn fn);

    template <typename T, typename... Args>
        requires std::constructible_from<T, Args&&...>
    T* allocate(Args&&... args) {
        return prism::allocate<T>(alloc, std::forward<Args>(args)...);
    }

    template <typename T, typename... Args>
        requires std::constructible_from<T, MonotonicBufferResource*, Args&&...>
    T* allocate(Args&&... args) {
        return prism::allocate<T>(alloc, &alloc, std::forward<Args>(args)...);
    }

    decltype(auto) withFacetState(FacetState s, ParserFn auto f) {
        FacetState stash = facetState;
        facetState = s;
        decltype(auto) result = invoke(f);
        facetState = stash;
        if constexpr (std::is_reference_v<decltype(result)>)
            return std::forward<decltype(result)>(result);
        else
            return result;
    }

    MonotonicBufferResource& alloc;
    SourceContext const& sourceCtx;
    IssueHandler& iss;
    Lexer lexer;
    std::optional<Token> peekToken;
    FacetState facetState = FacetState::General;
    bool inRecovery = false;
};

} // namespace

AstSourceFile* prism::parseSourceFile(MonotonicBufferResource& alloc,
                                      SourceContext const& sourceCtx,
                                      IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseSourceFile();
}

Facet const* prism::parseFacet(MonotonicBufferResource& alloc,
                               SourceContext const& sourceCtx,
                               IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseCommaFacet();
}

AstSourceFile* Parser::parseSourceFile() {
    return allocate<AstSourceFile>(sourceCtx,
                                   parseSequence(&Parser::parseDecl, End));
}

AstStmt* Parser::parseStmt() {
    if (auto decl = parseDecl()) {
        return decl;
    }
    if (auto stmt = parseExprStmt()) {
        return stmt;
    }
    return nullptr;
}

AstDecl* Parser::parseDecl() {
    if (auto function = parseFuncDecl()) {
        return function;
    }
    return nullptr;
}

AstFuncDecl* Parser::parseFuncDecl() {
    auto declarator = match(Function);
    if (!declarator) return nullptr;
    auto name = parseName();
    auto params = parseParamList();
    auto retType = match(Arrow) ? parseTypeSpec() : nullptr;
    auto body = parseCompoundStmt();
    return allocate<AstFuncDecl>(*declarator, std::move(name),
                                 std::move(params), std::move(retType),
                                 std::move(body));
}

AstCompoundStmt* Parser::parseCompoundStmt() {
    auto openBrace = match(OpenBrace);
    if (!openBrace) return nullptr;
    auto statements = parseSequence(&Parser::parseStmt, CloseBrace);
    auto closeBrace = match(CloseBrace).value_or(ErrorToken);
    return allocate<AstCompoundStmt>(*openBrace, closeBrace,
                                     std::move(statements));
}

AstParamDecl* Parser::parseParamDecl() {
    auto name = parseUnqualName();
    auto colon = match(Colon);
    auto typeSpec = colon ? parseTypeSpec() :
                            recover(&Parser::parseTypeSpec, { .stop = Comma });
    return allocate<AstParamDecl>(std::move(name), colon.value_or(ErrorToken),
                                  std::move(typeSpec));
}

AstParamList* Parser::parseParamList() {
    auto openParen = match(OpenParen);
    if (!openParen) return nullptr;
    auto seq = parseSequence(&Parser::parseParamDecl, CloseParen, Comma);
    auto closeParen = match(CloseParen).value_or(ErrorToken);
    return allocate<AstParamList>(*openParen, closeParen, std::move(seq));
}

AstExprStmt* Parser::parseExprStmt() {
    auto expr = parseExpr();
    if (!expr) return nullptr;
    if (!match(Semicolon)) {
        assert(false); // Push error
    }
    return allocate<AstExprStmt>(std::move(expr));
}

AstFacet* Parser::parseFacet() {
    return parseFacetImpl<AstFacet, AstRawFacet>(&Parser::parseCommaFacet);
}

AstExpr* Parser::parseExpr() {
    return parseFacetImpl<AstExpr, AstExprFacet>(&Parser::parseCommaFacet);
}

AstTypeSpec* Parser::parseTypeSpec() {
    return withFacetState(FacetState::Type, [this] {
        return parseFacetImpl<AstTypeSpec, AstTypeSpecFacet>(
            &Parser::parsePrefixFacet);
    });
}

static Token firstToken(Facet const* node) {
    PRISM_ASSERT(node);
    // clang-format off
    return csp::visit(*node, csp::overload{
        [](TerminalFacet const& node) { return node.token(); },
        [](NonTerminalFacet const& node) { return firstToken(node.childAt(0)); }
    });
    // clang-format on
}

template <typename Abstract, typename Concrete>
Abstract* Parser::parseFacetImpl(ParserFn auto start) {
    auto* facet = invoke(start);
    if (!facet) return nullptr;
    return allocate<Concrete>(facet, firstToken(facet));
}

// MARK: - Facets
Facet const* Parser::parseCommaFacet() {
    return parseBinaryFacetLTR(Comma, &Parser::parseAssignFacet);
}

Facet const* Parser::parseAssignFacet() {
    static constexpr TokenKind Ops[] = { Equal,       PlusEq,    MinusEq,
                                         StarEq,      SlashEq,   PercentEq,
                                         AmpersandEq, VertBarEq, CircumflexEq };
    return parseBinaryFacetRTL(Ops, &Parser::parseCastFacet);
}

Facet const* Parser::parseCastFacet() {
    Facet const* facet = parseTernCondFacet();
    if (!facet) return nullptr;
    while (true) {
        auto as = match(As);
        if (!as) return facet;
        auto* type = parsePrefixFacet();
        if (!type) {
            assert(false); // Expected type spec
        }
        facet = makeBinaryFacet(alloc, facet, *as, type);
    }
}

Facet const* Parser::parseTernCondFacet() {
    auto* cond = parseBinCondFacet();
    if (!cond) return nullptr;
    auto question = match(Question);
    if (!question) return cond;
    auto* lhs = parseOrRecover(&Parser::parseCommaFacet,
                               { .stop = { Colon, Semicolon } },
                               raiseCb<ExpectedExpression>());
    auto colon = match(Colon);
    auto* rhs = eval_as (Facet const*) {
        if (colon) {
            return parseOrRecover(&Parser::parseTernCondFacet,
                                  { .stop = Semicolon },
                                  raiseCb<ExpectedExpression>());
        }
        Token expColon = peek();
        if (auto* rhs = parseOrRecover(&Parser::parseTernCondFacet,
                                       { .stop = { Colon, Semicolon } }))
        {
            raise<ExpectedToken>(expColon, Colon);
            return rhs;
        }
        if ((colon = match(Colon))) {
            return parseOrRecover(&Parser::parseTernCondFacet,
                                  { .stop = Semicolon },
                                  raiseCb<ExpectedExpression>());
        }
        raise<ExpectedToken>(peek(), Colon);
        raise<ExpectedExpression>(peek());
        return nullptr;
    };
    return makeCondFacet(alloc, cond, *question, lhs,
                         colon.value_or(ErrorToken), rhs);
}

Facet const* Parser::parseBinCondFacet() {
    return parseBinaryFacetRTL(QuestionColon, &Parser::parseLogicalOrFacet);
}

Facet const* Parser::parseLogicalOrFacet() {
    return parseBinaryFacetLTR(DoubleVertBar, &Parser::parseLogicalAndFacet);
}

Facet const* Parser::parseLogicalAndFacet() {
    return parseBinaryFacetLTR(DoubleAmpersand, &Parser::parseOrFacet);
}

Facet const* Parser::parseOrFacet() {
    return parseBinaryFacetLTR(VertBar, &Parser::parseXorFacet);
}

Facet const* Parser::parseXorFacet() {
    return parseBinaryFacetLTR(Circumflex, &Parser::parseAndFacet);
}

Facet const* Parser::parseAndFacet() {
    return parseBinaryFacetLTR(Ampersand, &Parser::parseEqFacet);
}

Facet const* Parser::parseEqFacet() {
    return parseBinaryFacetLTR({ DoubleEqual, NotEq }, &Parser::parseRelFacet);
}

Facet const* Parser::parseRelFacet() {
    return parseBinaryFacetLTR({ LeftAngle, LeftAngleEq, RightAngle,
                                 RightAngleEq },
                               &Parser::parseShiftFacet);
}

Facet const* Parser::parseShiftFacet() {
    return parseBinaryFacetLTR({ DoubleLeftAngle, DoubleRightAngle },
                               &Parser::parseAddFacet);
}

Facet const* Parser::parseAddFacet() {
    return parseBinaryFacetLTR({ Plus, Minus }, &Parser::parseMulFacet);
}

Facet const* Parser::parseMulFacet() {
    return parseBinaryFacetLTR({ Star, Slash, Percent },
                               &Parser::parsePrefixFacet);
}

Facet const* Parser::parsePrefixFacet() {
    auto tok = match(Plus, Minus, Tilde, Exclam, DoublePlus, DoubleMinus, Mut,
                     Dyn, Star, Ampersand, Question, New, Move);
    if (!tok) return parsePostfixFacet();
    auto* operand = parsePrefixFacet();
    return makePrefixFacet(alloc, *tok, operand);
}

static TokenKind toClosing(TokenKind open) {
    switch (open) {
    case OpenParen:
        return CloseParen;
    case OpenBracket:
        return CloseBracket;
    case OpenBrace:
        return CloseBrace;
    default:
        PRISM_ASSERT(false);
    }
}

Facet const* Parser::parsePostfixFacet() {
    Facet const* operand = parsePrimaryFacet();
    if (!operand) return nullptr;
    while (true) {
        if (auto tok = match(DoublePlus, DoubleMinus)) {
            operand = makePostfixFacet(alloc, operand, *tok);
            continue;
        }
        constexpr TokenKind AllParenTypes[] = { OpenParen, OpenBracket,
                                                OpenBrace };
        auto parenTypes = facetState == FacetState::General ?
                              AllParenTypes :
                              std::span(AllParenTypes).subspan(0, 2);
        if (auto open = match(parenTypes)) {
            TokenKind closingKind = toClosing(open->kind);
            auto argList =
                parseSequence(&Parser::parseAssignFacet, closingKind, Comma);
            auto close = match(closingKind);
            PRISM_ASSERT(close);
            auto* args = makeListFacet(alloc, argList);
            operand = makeCallFacet(alloc, operand, *open, args, *close);
            continue;
        }
        return operand;
    }
}

Facet const* Parser::parsePrimaryFacet() {
    if (auto tok =
            match(Identifier, IntLiteralBin, IntLiteralDec, IntLiteralHex))
        return makeTerminal(alloc, *tok);
    if (auto tok = match(OpenParen)) {
        auto* facet = parseCommaFacet();
        if (!facet) {
            assert(false); // Expected facet
        }
        if (!match(CloseParen)) {
            assert(false); // Expected ')'
        }
        return facet;
    }
    return nullptr;
}

Facet const* Parser::parseFstringFacet() { return nullptr; }

Facet const* Parser::parseBinaryFacetLTR(
    VolatileList<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = invoke(next);
    if (!lhs) return nullptr;
    while (true) {
        auto tok = match(acceptedOperators);
        if (!tok) return lhs;
        auto* rhs = invoke(next);
        if (!rhs) {
            assert(false); // Expected facet
        }
        lhs = makeBinaryFacet(alloc, lhs, *tok, rhs);
    }
}

Facet const* Parser::parseBinaryFacetRTL(
    VolatileList<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = invoke(next);
    if (!lhs) return nullptr;
    if (auto tok = match(acceptedOperators)) {
        auto* rhs = parseBinaryFacetRTL(acceptedOperators, next);
        if (!rhs) {
            assert(false); // Expected facet
        }
        return makeBinaryFacet(alloc, lhs, *tok, rhs);
    }
    return lhs;
}

AstName* Parser::parseName() { return parseUnqualName(); }

AstUnqualName* Parser::parseUnqualName() {
    if (auto tok = match(Identifier)) {
        return allocate<AstUnqualName>(*tok);
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
        auto elem = invoke(parser);
        if (!elem) {
            assert(false); // Expected element
        }
        seq.push_back(std::move(elem));
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
InvokeResult<Fn> Parser::recover(Fn next, RecoveryOptions options) {
    if (inRecovery) {
        return {};
    }
    inRecovery = true;
    utl::scope_guard reset = [this] { inRecovery = false; };
    for (size_t i = 0; i < options.numAttempts; ++i) {
        if (ranges::contains(options.stop, peek().kind)) {
            return nullptr;
        }
        if (auto result = invoke(next)) {
            return result;
        }
        auto token = eat();
        raise<UnexpectedToken>(token);
    }
    return {};
}

template <ParserFn Fn>
bool Parser::validOrRecover(InvokeResult<Fn>& obj, Fn next,
                            RecoveryOptions options) {
    return obj || (obj = recover(next, options));
}

template <ParserFn Fn>
InvokeResult<Fn> Parser::parseOrRecover(Fn next, RecoveryOptions options) {
    auto obj = invoke(next);
    validOrRecover(obj, next, options);
    return obj;
}

template <ParserFn Fn>
InvokeResult<Fn> Parser::parseOrRecover(Fn next, RecoveryOptions options,
                                        ParserFn auto raiseIssue) {
    auto obj = parseOrRecover(next, options);
    if (!obj) invoke(raiseIssue);
    return obj;
}

template <ParserFn Fn>
InvokeResult<Fn> Parser::invoke(Fn fn) {
    if constexpr (std::invocable<Fn>)
        return std::invoke(fn);
    else
        return std::invoke(fn, *this);
}
