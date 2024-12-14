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
using ranges::views::transform;

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
    AstDecl* parseGlobalDecl();
    AstFuncDecl* parseFuncDecl();
    AstDecl* parseStructDecl();
    AstDecl* parseVarDecl();
    AstStmt* parseStmt();
    AstDecl* parseLocalDecl();
    AstParamDecl* parseParamDecl();
    AstParamList* parseParamList();
    AstExprStmt* parseExprStmt();
    AstName* parseName();
    AstUnqualName* parseUnqualName();
    AstFacet* parseFacet();
    AstExpr* parseExpr();
    AstCompoundExpr* parseCompoundExpr();
    AstTypeSpec* parseTypeSpec();
    template <typename Concrete, typename Abstract = Concrete>
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
    Facet const* parseCallFacet(Facet const* primary);
    Facet const* parsePrimaryFacet();
    Facet const* parseFstringFacet();
    CompoundFacet const* parseCompoundFacet();
    Facet const* parseClosureOrFnTypeFacet();

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

static Token firstToken(Facet const* node) {
    PRISM_ASSERT(node);
    // clang-format off
    return csp::visit(*node, csp::overload{
        [](TerminalFacet const& node) { return node.token(); },
        [](NonTerminalFacet const& node) { return firstToken(node.childAt(0)); },
        [](AstWrapperFacet const& node) { return node.get()->firstToken(); }
    });
    // clang-format on
}

AstSourceFile* Parser::parseSourceFile() {
    return allocate<AstSourceFile>(sourceCtx,
                                   parseSequence(&Parser::parseGlobalDecl,
                                                 End));
}

AstDecl* Parser::parseGlobalDecl() {
    if (auto fn = parseFuncDecl()) return fn;
    if (auto str = parseStructDecl()) return str;
    if (auto var = parseVarDecl()) return var;
    return nullptr;
}

AstFuncDecl* Parser::parseFuncDecl() {
    auto declarator = match(Function);
    if (!declarator) return nullptr;
    auto* name = parseName();
    auto* params = parseParamList();
    auto* retType = match(Arrow) ? parseTypeSpec() : nullptr;
    auto* body = parseCompoundExpr();
    return allocate<AstFuncDecl>(*declarator, name, params, retType, body);
}

AstDecl* Parser::parseStructDecl() { return nullptr; }

AstDecl* Parser::parseVarDecl() {
    auto declarator = match(Var, Let);
    if (!declarator) return nullptr;
    auto* name = parseName();
    if (!name) {
        assert(false); // Expected name
    }
    auto colon = match(Colon);
    auto* typeSpec = colon ? parseTypeSpec() : nullptr;
    auto eq = match(Equal);
    auto* initExpr = eq ? parseExpr() : nullptr;
    if (!match(Semicolon)) raise<ExpectedToken>(peek(), Semicolon);
    return allocate<AstVarDecl>(*declarator, name, colon, typeSpec, eq,
                                initExpr);
}

AstStmt* Parser::parseStmt() {
    if (auto decl = parseLocalDecl()) {
        return decl;
    }
    if (auto stmt = parseExprStmt()) {
        return stmt;
    }
    if (auto tok = match(Semicolon)) {
        return allocate<AstEmptyStmt>(*tok);
    }
    return nullptr;
}

AstDecl* Parser::parseLocalDecl() {
    if (auto var = parseVarDecl()) return var;
    return nullptr;
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
    if (auto* expr = parseCompoundExpr()) return allocate<AstExprStmt>(expr);
    auto* expr = parseExpr();
    if (!expr) return nullptr;
    if (!match(Semicolon)) {
        assert(false); // Push error
    }
    return allocate<AstExprStmt>(expr);
}

AstFacet* Parser::parseFacet() {
    return parseFacetImpl<AstRawFacet, AstFacet>(&Parser::parseCommaFacet);
}

static AstExpr* unwrapFacet(AstFacetExpr* expr) {
    if (expr)
        if (auto* wrapper = csp::dyncast<AstWrapperFacet const*>(expr->facet()))
            if (auto* wrapped = csp::dyncast<AstExpr*>(wrapper->get()))
                return wrapped;
    return expr;
}

AstExpr* Parser::parseExpr() {
    auto* expr = parseFacetImpl<AstFacetExpr>(&Parser::parseCommaFacet);
    return unwrapFacet(expr);
}

AstCompoundExpr* Parser::parseCompoundExpr() {
    auto* compound = parseCompoundFacet();
    if (!compound) return nullptr;
    auto stmts = compound->statements()->children() |
                 transform(csp::cast<AstWrapperFacet const*>) |
                 transform(&AstWrapperFacet::get) |
                 transform(csp::cast<AstStmt*>) |
                 ranges::to<utl::small_vector<AstStmt*>>;
    if (auto* retFct = compound->returnFacet()) {
        auto* expr = allocate<AstFacetExpr>(retFct, firstToken(retFct));
        auto* stmt = allocate<AstYieldStmt>(expr);
        stmts.push_back(stmt);
    }
    return allocate<AstCompoundExpr>(compound->openBrace()->token(), stmts);
}

AstTypeSpec* Parser::parseTypeSpec() {
    return withFacetState(FacetState::Type, [this] {
        return parseFacetImpl<AstTypeSpec>(&Parser::parsePrefixFacet);
    });
}

template <typename Concrete, typename Abstract>
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
        if (auto* call = parseCallFacet(operand)) {
            operand = call;
            continue;
        }
        return operand;
    }
}

Facet const* Parser::parseCallFacet(Facet const* primary) {
    if (csp::isa<CompoundFacet>(primary)) return nullptr;
    static constexpr TokenKind AllParenTypes[] = { OpenParen, OpenBracket,
                                                   OpenBrace };
    auto parenTypes = facetState == FacetState::General ?
                          AllParenTypes :
                          std::span(AllParenTypes).subspan(0, 2);
    auto open = match(parenTypes);
    if (!open) return nullptr;
    TokenKind closingKind = toClosing(open->kind);
    auto argList = parseSequence(&Parser::parseAssignFacet, closingKind, Comma);
    auto close = match(closingKind);
    PRISM_ASSERT(close);
    auto* args = makeListFacet(alloc, argList);
    return makeCallFacet(alloc, primary, *open, args, *close);
}

Facet const* Parser::parsePrimaryFacet() {
    if (auto tok = match(Identifier, IntLiteralBin, IntLiteralDec,
                         IntLiteralHex, True, False, This, AutoArg, Void, Int,
                         Double))
        return makeTerminal(alloc, *tok);
    if (auto* closure = parseClosureOrFnTypeFacet()) return closure;
    if (auto open = match(OpenParen)) {
        auto* facet = parseCommaFacet();
        if (!facet) {
            assert(false); // Expected facet
        }
        if (!match(CloseParen)) {
            assert(false); // Expected ')'
        }
        return facet;
    }
    if (auto open = match(OpenBracket)) {
        assert(false); // Unimplemented
        if (auto close = match(CloseBracket)) {
        }
    }
    if (auto* cmpFacet = parseCompoundFacet()) return cmpFacet;
    return nullptr;
}

CompoundFacet const* Parser::parseCompoundFacet() {
    auto open = match(OpenBrace);
    if (!open) return nullptr;
    utl::small_vector<Facet const*> elems;
    auto* returnFacet = eval_as (Facet const*) {
        while (true) {
            Facet const* facet = parseCommaFacet();
            if (facet) {
                if (peekMatch(CloseBrace)) return facet;
                auto semicolon = match(Semicolon);
                if (!semicolon) raise<ExpectedToken>(peek(), Semicolon);
                auto* expr = allocate<AstFacetExpr>(facet, firstToken(facet));
                auto* stmt = allocate<AstExprStmt>(expr);
                elems.push_back(allocate<AstWrapperFacet>(stmt));
                continue;
            }
            if (auto* stmt = parseStmt())
                elems.push_back(allocate<AstWrapperFacet>(stmt));
            else
                return nullptr;
        }
    };
    auto close = match(CloseBrace);
    if (!close) raise<ExpectedToken>(peek(), CloseBrace);
    return makeCompoundFacet(alloc, *open, makeListFacet(alloc, elems),
                             returnFacet, close.value_or(ErrorToken));
}

Facet const* Parser::parseClosureOrFnTypeFacet() {
    auto fn = match(Function);
    if (!fn) return nullptr;
    auto* params = parseParamList();
    auto arrow = match(Arrow);
    auto* retType = arrow ? parseTypeSpec() : nullptr;
    if (facetState != FacetState::Type) {
        if (auto* body = parseExpr()) {
            auto* closure =
                allocate<AstClosureExpr>(*fn, params, retType, body);
            return allocate<AstWrapperFacet>(closure);
        }
    }
    if (!params || !retType) {
        assert(false); // Invalid fn type
    }
    return makeFnTypeFacet(alloc, *fn, allocate<AstWrapperFacet>(params),
                           arrow.value_or(ErrorToken),
                           allocate<AstWrapperFacet>(retType));
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
