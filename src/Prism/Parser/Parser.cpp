#include "Prism/Parser/Parser.h"

#include "Prism/Parser/LinearParser.h"

#include "Prism/Common/SyntaxMacros.h"

using namespace prism;

using enum TokenKind;

static constexpr Token ErrorToken = Token::ErrorToken;

namespace {

template <typename Fn>
concept ParserFn = std::invocable<Fn>;

template <ParserFn Fn>
using InvokeResult = std::invoke_result_t<Fn>;

enum class FacetState { General, Type };

struct Parser: LinearParser {
    using LinearParser::LinearParser;

    SourceFileFacet const* parseSourceFile();
    DeclFacet const* parseGlobalDecl();
    FuncDeclFacet const* parseFuncDecl();
    CompTypeDeclFacet const* parseCompTypeDecl();
    BaseDeclFacet const* parseBaseDecl();
    BaseListFacet const* parseBaseList();
    MemberListFacet const* parseMemberList();
    VarDeclFacet const* parseVarDecl();
    StmtFacet const* parseStmt();
    DeclFacet const* parseLocalDecl();
    ParamDeclFacet const* parseParamDecl();
    ParamListFacet const* parseParamList();
    ReturnStmtFacet const* parseReturnStmt();
    EmptyStmtFacet const* parseEmptyStmt();
    ExprStmtFacet const* parseExprStmt();

    Facet const* parseName();
    Facet const* parseUnqualName();
    Facet const* parseFacet();
    Facet const* parseExpr();
    Facet const* parseTypeSpec();

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
    Facet const* parseParenthesisedFacet();
    Facet const* parseArrayFacet();
    Facet const* parseListFacet(TokenKind delim, TokenKind end);

    Facet const* parseBinaryFacetLTR(
        VolatileList<TokenKind const> acceptedOperators, ParserFn auto next);
    Facet const* parseBinaryFacetRTL(
        VolatileList<TokenKind const> acceptedOperators, ParserFn auto next);

    template <ParserFn Fn>
    utl::small_vector<InvokeResult<Fn>> parseSequence(
        Fn parser, std::invocable<Token> auto error, TokenKind end,
        std::optional<TokenKind> delim = std::nullopt);

    decltype(auto) withFacetState(FacetState s, ParserFn auto f) {
        FacetState stash = facetState;
        facetState = s;
        decltype(auto) result = f();
        facetState = stash;
        if constexpr (std::is_reference_v<decltype(result)>)
            return std::forward<decltype(result)>(result);
        else
            return result;
    }

    ///
    LinParser<0> makeParser() {
        static constexpr auto stop = [](Token tok) {
            static constexpr std::array kinds = {
                Var,   Let, Fn, Struct, Trait,      Return,   For,
                While, Do,  If, Else,   CloseBrace, Semicolon
            };
            return ranges::contains(kinds, tok.kind);
        };
        return parseLin({ stop });
    }

    /// Matches \p kind and raises an expected token error if not matched
    std::optional<Token> matchExpect(TokenKind kind) {
        if (auto token = match(kind)) return token;
        raise<ExpectedToken>(peek(), kind);
        return std::nullopt;
    }

    /// Parser rule that is equivalent to the lowercase `matchExpect()`
    ParserRule MatchExpect(TokenKind kind) {
        return { Match(kind), Raise<ExpectedToken>(kind) };
    }

    FacetState facetState = FacetState::General;
};

} // namespace

#define fn(Name, ...)    [this] { return Name(__VA_ARGS__); }
#define valfn(Name, ...) [=, this] { return Name(__VA_ARGS__); }

SourceFileFacet const* prism::parseSourceFile(MonotonicBufferResource& alloc,
                                              SourceContext const& sourceCtx,
                                              IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseSourceFile();
}

Facet const* prism::parseFacet(MonotonicBufferResource& alloc,
                               SourceContext const& sourceCtx,
                               IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseAssignFacet();
}

SourceFileFacet const* Parser::parseSourceFile() {
    return allocate<SourceFileFacet>(
        parseSequence(fn(parseGlobalDecl), Raise<ExpectedDecl>(), End));
}

DeclFacet const* Parser::parseGlobalDecl() {
    if (auto fn = parseFuncDecl()) return fn;
    if (auto str = parseCompTypeDecl()) return str;
    if (auto var = parseVarDecl()) return var;
    return nullptr;
}

FuncDeclFacet const* Parser::parseFuncDecl() {
    auto [declarator, name, params, arrow, retType, body] =
        makeParser()
            .rule(Match(Fn))
            .rule({ fn(parseName), Raise<ExpectedDeclName>() })
            .rule({ fn(parseParamList), Raise<ExpectedParamList>() })
            .optRule({ Match(Arrow),
                       { fn(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .rule({ fn(parseCompoundFacet), Raise<ExpectedCompoundFacet>() })
            .eval();
    if (!declarator) return nullptr;
    return allocate<FuncDeclFacet>(declarator, name, params, arrow, retType,
                                   body);
}

CompTypeDeclFacet const* Parser::parseCompTypeDecl() {
    auto [declarator, name, colon, baselist, openbrace, body, closebrace] =
        makeParser()
            .rule(Match(Struct, Trait))
            .rule({ fn(parseName), Raise<ExpectedDeclName>() })
            .optRule({ Match(Colon), fn(parseBaseList) })
            .rule(MatchExpect(OpenBrace))
            .rule(fn(parseMemberList))
            .rule(MatchExpect(CloseBrace))
            .eval();
    if (!declarator) return nullptr;
    return allocate<CompTypeDeclFacet>(declarator, name, colon, baselist,
                                       openbrace, body, closebrace);
}

BaseDeclFacet const* Parser::parseBaseDecl() {
    auto* type = parseTypeSpec();
    return allocate<BaseDeclFacet>(ErrorToken, nullptr, ErrorToken, type);
}

BaseListFacet const* Parser::parseBaseList() {
    auto elems = parseSequence(fn(parseBaseDecl), Raise<ExpectedBaseDecl>(),
                               OpenBrace, Comma);
    if (elems.empty()) raise<ExpectedBaseDecl>(peek());
    return allocate<BaseListFacet>(elems);
}

MemberListFacet const* Parser::parseMemberList() {
    auto elems =
        parseSequence(fn(parseGlobalDecl), Raise<ExpectedDecl>(), CloseBrace);
    return allocate<MemberListFacet>(elems);
}

VarDeclFacet const* Parser::parseVarDecl() {
    auto [declarator, name, colon, type, assign, init, semicolon] =
        makeParser()
            .rule(Match(Var, Let))
            .rule(fn(parseName))
            .optRule({ Match(Colon),
                       { fn(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .optRule({ Match(Equal), { fn(parseExpr), Raise<ExpectedExpr>() } })
            .rule(MatchExpect(Semicolon))
            .eval();
    if (!declarator) return nullptr;
    return allocate<VarDeclFacet>(declarator, name, colon, type, assign, init,
                                  semicolon);
}

StmtFacet const* Parser::parseStmt() {
    if (auto* decl = parseLocalDecl()) return decl;
    if (auto* stmt = parseReturnStmt()) return stmt;
    if (auto* stmt = parseExprStmt()) return stmt;
    if (auto* stmt = parseEmptyStmt()) return stmt;
    return nullptr;
}

DeclFacet const* Parser::parseLocalDecl() {
    if (auto var = parseVarDecl()) return var;
    return nullptr;
}

ParamDeclFacet const* Parser::parseParamDecl() {
    auto* name = parseUnqualName();
    auto colon = match(Colon);
    auto* type = colon ? parseTypeSpec() : nullptr;
    return allocate<ParamDeclFacet>(ErrorToken, name,
                                    colon.value_or(ErrorToken), type);
}

ParamListFacet const* Parser::parseParamList() {
    if (!match(OpenParen)) return nullptr;
    auto seq = parseSequence(fn(parseParamDecl), Raise<ExpectedParamDecl>(),
                             CloseParen, Comma);
    match(CloseParen);
    return allocate<ParamListFacet>(seq);
}

ReturnStmtFacet const* Parser::parseReturnStmt() {
    auto [ret, expr, semicolon] = makeParser()
                                      .rule(Match(Return))
                                      .optRule({ fn(parseExpr) })
                                      .rule(MatchExpect(Semicolon))
                                      .eval();
    if (!ret) return nullptr;
    return allocate<ReturnStmtFacet>(ret, expr, semicolon);
}

EmptyStmtFacet const* Parser::parseEmptyStmt() {
    if (auto tok = match(Semicolon)) return allocate<EmptyStmtFacet>(*tok);
    return nullptr;
}

ExprStmtFacet const* Parser::parseExprStmt() {
    if (auto* expr = parseCompoundFacet())
        return allocate<ExprStmtFacet>(expr, nullptr);
    auto [expr, semicolon] =
        makeParser().rule(fn(parseExpr)).rule(MatchExpect(Semicolon)).eval();
    if (!expr) return nullptr;
    return allocate<ExprStmtFacet>(expr, semicolon);
}

Facet const* Parser::parseFacet() { return parseAssignFacet(); }

Facet const* Parser::parseExpr() { return parseFacet(); }

Facet const* Parser::parseTypeSpec() {
    return withFacetState(FacetState::Type, [&] { return parsePrefixFacet(); });
}

Facet const* Parser::parseAssignFacet() {
    static constexpr TokenKind Ops[] = { Equal,       PlusEq,    MinusEq,
                                         StarEq,      SlashEq,   PercentEq,
                                         AmpersandEq, VertBarEq, CircumflexEq };
    return parseBinaryFacetRTL(Ops, fn(parseCastFacet));
}

Facet const* Parser::parseCastFacet() {
    Facet const* facet = parseTernCondFacet();
    if (!facet) return nullptr;
    while (true) {
        auto [as, type] =
            makeParser()
                .rule(Match(As))
                .rule({ fn(parsePrefixFacet), Raise<ExpectedTypeSpec>() })
                .eval();
        if (!as) return facet;
        facet = allocate<BinaryFacet>(facet, as, type);
    }
}

Facet const* Parser::parseTernCondFacet() {
    auto* cond = parseBinCondFacet();
    if (!cond) return nullptr;
    auto [question, lhs, colon, rhs] =
        makeParser()
            .rule(Match(Question))
            .rule({ fn(parseAssignFacet), Raise<ExpectedExpr>() })
            .rule(MatchExpect(Colon))
            .rule({ fn(parseTernCondFacet), Raise<ExpectedExpr>() })
            .eval();
    if (!question) return cond;
    return allocate<CondFacet>(cond, question, lhs, colon, rhs);
}

Facet const* Parser::parseBinCondFacet() {
    return parseBinaryFacetRTL(QuestionColon, fn(parseLogicalOrFacet));
}

Facet const* Parser::parseLogicalOrFacet() {
    return parseBinaryFacetLTR(DoubleVertBar, fn(parseLogicalAndFacet));
}

Facet const* Parser::parseLogicalAndFacet() {
    return parseBinaryFacetLTR(DoubleAmpersand, fn(parseOrFacet));
}

Facet const* Parser::parseOrFacet() {
    return parseBinaryFacetLTR(VertBar, fn(parseXorFacet));
}

Facet const* Parser::parseXorFacet() {
    return parseBinaryFacetLTR(Circumflex, fn(parseAndFacet));
}

Facet const* Parser::parseAndFacet() {
    return parseBinaryFacetLTR(Ampersand, fn(parseEqFacet));
}

Facet const* Parser::parseEqFacet() {
    return parseBinaryFacetLTR({ DoubleEqual, NotEq }, fn(parseRelFacet));
}

Facet const* Parser::parseRelFacet() {
    return parseBinaryFacetLTR({ LeftAngle, LeftAngleEq, RightAngle,
                                 RightAngleEq },
                               fn(parseShiftFacet));
}

Facet const* Parser::parseShiftFacet() {
    return parseBinaryFacetLTR({ DoubleLeftAngle, DoubleRightAngle },
                               fn(parseAddFacet));
}

Facet const* Parser::parseAddFacet() {
    return parseBinaryFacetLTR({ Plus, Minus }, fn(parseMulFacet));
}

Facet const* Parser::parseMulFacet() {
    return parseBinaryFacetLTR({ Star, Slash, Percent }, fn(parsePrefixFacet));
}

Facet const* Parser::parsePrefixFacet() {
    static constexpr std::array Ops = {
        Plus, Minus, Tilde,     Exclam,   DoublePlus, DoubleMinus, Mut,
        Dyn,  Star,  Ampersand, Question, New,        Move
    };
    auto [op, operand] =
        makeParser()
            .rule(Match(Ops))
            .rule({ fn(parsePrefixFacet), Raise<ExpectedExpr>() })
            .eval();
    if (!op) return parsePostfixFacet();
    return allocate<PrefixFacet>(op, operand);
}

Facet const* Parser::parsePostfixFacet() {
    Facet const* operand = parsePrimaryFacet();
    if (!operand) return nullptr;
    while (true) {
        if (auto tok = match({ DoublePlus, DoubleMinus })) {
            operand = allocate<PostfixFacet>(operand, *tok);
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
    if (isa<CompoundFacet>(primary)) return nullptr;
    auto impl = [&](TokenKind openKind, TokenKind closeKind) -> Facet const* {
        auto [open, args, close] =
            makeParser()
                .rule(Match(openKind))
                .rule(valfn(parseListFacet, Comma, closeKind))
                .rule(MatchExpect(closeKind))
                .eval();
        if (!open) return nullptr;
        return allocate<CallFacet>(primary, open, args, close);
    };
    if (auto* call = impl(OpenParen, CloseParen)) return call;
    if (auto* call = impl(OpenBracket, CloseBracket)) return call;
    if (facetState != FacetState::Type)
        if (auto* call = impl(OpenBrace, CloseBrace)) return call;
    return nullptr;
}

Facet const* Parser::parsePrimaryFacet() {
    static constexpr std::array TermKinds = {
        Identifier, IntLiteralBin, IntLiteralDec, IntLiteralHex, True,  False,
        This,       AutoArg,       Void,          Int,           Double
    };
    if (auto tok = match(TermKinds)) return allocate<TerminalFacet>(*tok);
    if (auto* closure = parseClosureOrFnTypeFacet()) return closure;
    if (auto* facet = parseParenthesisedFacet()) return facet;
    if (auto* array = parseArrayFacet()) return array;
    if (auto* cmpFacet = parseCompoundFacet()) return cmpFacet;
    return nullptr;
}

CompoundFacet const* Parser::parseCompoundFacet() {
    auto open = match(OpenBrace);
    if (!open) return nullptr;
    utl::small_vector<StmtFacet const*> elems;
    auto* returnFacet = EVAL_AS(Facet const*) {
        while (true) {
            Facet const* facet = parseAssignFacet();
            if (facet) {
                if (peekMatch(CloseBrace)) return facet;
                std::optional<Token> semicolon;
                if (!isa<CompoundFacet>(facet)) {
                    semicolon = matchExpect(Semicolon);
                }
                auto* stmt = allocate<ExprStmtFacet>(facet, semicolon.value_or(
                                                                ErrorToken));
                elems.push_back(stmt);
                continue;
            }
            if (auto* stmt = parseStmt())
                elems.push_back(stmt);
            else
                return nullptr;
        }
    };
    auto close = matchExpect(CloseBrace);
    return allocate<CompoundFacet>(*open, allocate<StmtListFacet>(elems),
                                   returnFacet, close.value_or(ErrorToken));
}

Facet const* Parser::parseClosureOrFnTypeFacet() {
    auto bodyIfNotType = [this] {
        return facetState != FacetState::Type ? parseExpr() : nullptr;
    };
    auto [fn, params, arrow, retType, body] =
        makeParser()
            .rule(Match(Fn))
            .optRule({ fn(parseParamList) })
            .optRule({ Match(Arrow), fn(parseTypeSpec) })
            .optRule({ bodyIfNotType })
            .eval();
    if (!fn) return nullptr;
    if (body) return allocate<ClosureFacet>(fn, params, arrow, retType, body);
    return allocate<FnTypeFacet>(fn, params, arrow, retType);
}

Facet const* Parser::parseParenthesisedFacet() {
    auto [open, facet, close] =
        makeParser()
            .rule(Match(OpenParen))
            .rule({ fn(parseFacet), Raise<ExpectedExpr>() })
            .rule(Match(CloseParen))
            .eval();
    if (!open) return nullptr;
    return allocate<ParenthesisedFacet>(open, facet, close);
}

Facet const* Parser::parseArrayFacet() {
    auto listParser = [this] { return parseListFacet(Comma, CloseBracket); };
    auto [open, list, close] = makeParser()
                                   .rule(Match(OpenBracket))
                                   .rule(listParser)
                                   .rule(Match(CloseParen))
                                   .eval();
    if (!open) return nullptr;
    return allocate<ArrayFacet>(open, list, close);
}

Facet const* Parser::parseListFacet(TokenKind delim, TokenKind end) {
    auto argList =
        parseSequence(fn(parseAssignFacet), Raise<ExpectedExpr>(), end, delim);
    return allocate<ListFacet>(argList);
}

Facet const* Parser::parseFstringFacet() { return nullptr; }

Facet const* Parser::parseBinaryFacetLTR(
    VolatileList<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = next();
    if (!lhs) return nullptr;
    while (true) {
        auto [op, rhs] = makeParser()
                             .rule(Match(acceptedOperators))
                             .rule({ next, Raise<ExpectedExpr>() })
                             .eval();
        if (!op) return lhs;
        lhs = allocate<BinaryFacet>(lhs, op, rhs);
    }
}

Facet const* Parser::parseBinaryFacetRTL(
    VolatileList<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = next();
    if (!lhs) return nullptr;
    auto [op, rhs] =
        makeParser()
            .rule(Match(acceptedOperators))
            .rule({ valfn(parseBinaryFacetRTL, acceptedOperators, next),
                    Raise<ExpectedExpr>() })
            .eval();
    if (!op) return lhs;
    return allocate<BinaryFacet>(lhs, op, rhs);
}

Facet const* Parser::parseName() { return parseUnqualName(); }

Facet const* Parser::parseUnqualName() {
    if (auto tok = match(Identifier)) {
        return allocate<TerminalFacet>(*tok);
    }
    return nullptr;
}

template <ParserFn Fn>
utl::small_vector<InvokeResult<Fn>> Parser::parseSequence(
    Fn parser, std::invocable<Token> auto error, TokenKind end,
    std::optional<TokenKind> delim) {
    utl::small_vector<InvokeResult<Fn>> seq;
    bool first = true;
    while (true) {
        if (peekMatch(end)) return seq;
        if (!first && delim && !matchExpect(*delim)) {
            return seq;
        }
        first = false;
        if (auto* elem = parser()) {
            seq.push_back(elem);
            continue;
        }
        error(peek());
        if (eat().kind == End) return seq;
    }
}
