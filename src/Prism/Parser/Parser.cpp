#include "Prism/Parser/Parser.h"

#include "Prism/Parser/ParserBase.h"

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

struct Parser: ParserBase {
    using ParserBase::ParserBase;

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

    template <ParserFn Fn>
    InvokeResult<Fn> invoke(Fn&& fn) {
        return fn();
    }

    FacetState facetState = FacetState::General;
};

} // namespace

#define fn(Name) [this] { return Name(); }

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
    return parser.parseCommaFacet();
}

SourceFileFacet const* Parser::parseSourceFile() {
    return allocate<SourceFileFacet>(parseSequence(fn(parseGlobalDecl), End));
}

DeclFacet const* Parser::parseGlobalDecl() {
    if (auto fn = parseFuncDecl()) return fn;
    if (auto str = parseCompTypeDecl()) return str;
    if (auto var = parseVarDecl()) return var;
    return nullptr;
}

FuncDeclFacet const* Parser::parseFuncDecl() {
    auto declarator = match(Function);
    if (!declarator) return nullptr;
    auto* name = parseName();
    auto* params = parseParamList();
    auto arrow = match(Arrow);
    auto* retType = arrow ? parseTypeSpec() : nullptr;
    auto* body = parseCompoundFacet();
    return allocate<FuncDeclFacet>(*declarator, name, params,
                                   arrow.value_or(ErrorToken), retType, body);
}

CompTypeDeclFacet const* Parser::parseCompTypeDecl() {
    auto declarator = match({ Struct, Trait });
    if (!declarator) return nullptr;
    auto* name = parseName();
    assert(name);
    auto colon = match(Colon);
    auto* baseList = colon ? parseBaseList() : nullptr;
    auto openBrace = match(OpenBrace);
    assert(openBrace);
    auto* body = parseMemberList();
    auto closeBrace = match(CloseBrace);
    assert(closeBrace);
    return allocate<CompTypeDeclFacet>(*declarator, name,
                                       colon.value_or(ErrorToken), baseList,
                                       openBrace.value_or(ErrorToken), body,
                                       closeBrace.value_or(ErrorToken));
}

BaseDeclFacet const* Parser::parseBaseDecl() {
    auto* type = parseTypeSpec();
    return allocate<BaseDeclFacet>(ErrorToken, nullptr, ErrorToken, type);
}

BaseListFacet const* Parser::parseBaseList() {
    auto elems = parseSequence(fn(parseBaseDecl), OpenBrace, Comma);
    return allocate<BaseListFacet>(elems);
}

MemberListFacet const* Parser::parseMemberList() {
    auto elems = parseSequence(fn(parseGlobalDecl), CloseBrace);
    return allocate<MemberListFacet>(elems);
}

#if 0
VarDeclFacet const* Parser::parseVarDecl() {
    auto declarator = match(Var, Let);
    if (!declarator) return nullptr;
    auto* name = parseName();
    if (!name) {
        assert(false); // Expected name
    }
    auto colon = match(Colon);
    auto* typeSpec = colon ? parseTypeSpec() : nullptr;
    auto assign = match(Equal);
    auto* initExpr = assign ? parseExpr() : nullptr;
    auto semicolon = match(Semicolon);
    if (!semicolon) raise<ExpectedToken>(peek(), Semicolon);
    return allocate<VarDeclFacet>(*declarator, name, colon.value_or(ErrorToken),
                                  typeSpec, assign.value_or(ErrorToken),
                                  initExpr, semicolon.value_or(ErrorToken));
}
#endif

static bool stmtStopCond(Token tok) {
    static constexpr std::array kinds = { Var,   Let,        Function, Struct,
                                          Trait, CloseBrace, Semicolon };
    return ranges::contains(kinds, tok.kind);
}

static bool stmtStopCondSemi(Token tok) {
    return stmtStopCond(tok) || tok.kind == Semicolon;
}

VarDeclFacet const* Parser::parseVarDecl() {
    auto [declarator, name, type, init, semicolon] =
        parseLin({ .isStop = stmtStopCondSemi })
            .rule(matcher(Var, Let))
            .rule(fn(parseName))
            .optRule({
                matcher(Colon),
                { fn(parseTypeSpec), raiser<ExpectedTypeSpec>(), { true } },
            })
            .optRule({
                matcher(Equal),
                { fn(parseExpr), raiser<ExpectedExpression>(), { true } },
            })
            .rule({ matcher(Semicolon),
                    raiser<ExpectedToken>(Semicolon),
                    { .backtrackIfFailed = true } })
            .eval();
    if (!declarator) return nullptr;
    auto [colon, typespec] = unpack<2>(type);
    auto [assign, initExpr] = unpack<2>(init);
    return allocate<VarDeclFacet>(declarator, name, colon, typespec, assign,
                                  initExpr, semicolon);
}
#if 0
VarDeclFacet const* Parser::parseVarDecl() {
    // clang-format off
    auto [declarator, name, type, init, semicolon] = parseLinearGrammar({
        matcher(Var, Let),
        fn(parseName),
        option({
            matcher(Colon),
            { fn(parseTypeSpec), raiser<ExpectedTypeSpec>(), { true } },
        }),
        option({
            matcher(Equal),
            { fn(parseExpr), raiser<ExpectedExpression>(), { true } },
        }),
        {
            matcher(Semicolon),
            raiser<ExpectedToken>(Semicolon),
            { .backtrackIfFailed = true }
        }
    }, { stmtStopCondSemi }); // clang-format on
    if (!declarator) return nullptr;
    auto [colon, typespec] = unpack<2>(type);
    auto [assign, initExpr] = unpack<2>(init);
    return allocate<VarDeclFacet>(declarator, name, colon, typespec, assign,
                                  initExpr, semicolon);
}
#endif
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
    auto seq = parseSequence(fn(parseParamDecl), CloseParen, Comma);
    match(CloseParen);
    return allocate<ParamListFacet>(seq);
}

ReturnStmtFacet const* Parser::parseReturnStmt() {
    auto [ret, expr, semicolon] =
        parseLinearGrammar({ matcher(Return),
                             option({ fn(parseExpr) }),
                             { matcher(Semicolon),
                               raiser<ExpectedToken>(Semicolon),
                               { .backtrackIfFailed = true } } },
                           { stmtStopCondSemi });
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
        parseLinearGrammar({ fn(parseExpr),
                             { matcher(Semicolon),
                               raiser<ExpectedToken>(Semicolon),
                               { .backtrackIfFailed = true } } },
                           { stmtStopCondSemi });
    if (!expr) return nullptr;
    return allocate<ExprStmtFacet>(expr, semicolon);
}

Facet const* Parser::parseFacet() { return parseCommaFacet(); }

Facet const* Parser::parseExpr() { return parseFacet(); }

Facet const* Parser::parseTypeSpec() {
    return withFacetState(FacetState::Type, [&] { return parsePrefixFacet(); });
}

Facet const* Parser::parseCommaFacet() {
    return parseBinaryFacetLTR(Comma, fn(parseAssignFacet));
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
        auto as = match(As);
        if (!as) return facet;
        auto* type = parsePrefixFacet();
        if (!type) {
            assert(false); // Expected type spec
        }
        facet = allocate<BinaryFacet>(facet, *as, type);
    }
}

Facet const* Parser::parseTernCondFacet() {
    auto* cond = parseBinCondFacet();
    if (!cond) return nullptr;
    auto question = match(Question);
    if (!question) return cond;
    auto* lhs = parseCommaFacet();
    auto colon = match(Colon);
    auto* rhs = parseTernCondFacet();
    return allocate<CondFacet>(cond, *question, lhs, colon.value_or(ErrorToken),
                               rhs);
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
    auto tok = match(Ops);
    if (!tok) return parsePostfixFacet();
    auto* operand = parsePrefixFacet();
    return allocate<PrefixFacet>(*tok, operand);
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
    static constexpr TokenKind AllParenTypes[] = { OpenParen, OpenBracket,
                                                   OpenBrace };
    auto parenTypes = facetState == FacetState::General ?
                          AllParenTypes :
                          std::span(AllParenTypes).subspan(0, 2);
    auto open = match(parenTypes);
    if (!open) return nullptr;
    TokenKind closingKind = toClosing(open->kind);
    auto argList = parseSequence(fn(parseAssignFacet), closingKind, Comma);
    auto close = match(closingKind);
    PRISM_ASSERT(close);
    auto* args = allocate<ListFacet>(argList);
    return allocate<CallFacet>(primary, *open, args, *close);
}

Facet const* Parser::parsePrimaryFacet() {
    static constexpr std::array TermKinds = {
        Identifier, IntLiteralBin, IntLiteralDec, IntLiteralHex, True,  False,
        This,       AutoArg,       Void,          Int,           Double
    };
    if (auto tok = match(TermKinds)) return allocate<TerminalFacet>(*tok);
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
    utl::small_vector<StmtFacet const*> elems;
    auto* returnFacet = EVAL_AS(Facet const*) {
        while (true) {
            Facet const* facet = parseCommaFacet();
            if (facet) {
                if (peekMatch(CloseBrace)) return facet;
                auto semicolon = match(Semicolon);
                if (!semicolon) raise<ExpectedToken>(peek(), Semicolon);
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
    auto close = match(CloseBrace);
    if (!close) raise<ExpectedToken>(peek(), CloseBrace);
    return allocate<CompoundFacet>(*open, allocate<StmtListFacet>(elems),
                                   returnFacet, close.value_or(ErrorToken));
}

Facet const* Parser::parseClosureOrFnTypeFacet() {
    auto fn = match(Function);
    if (!fn) return nullptr;
    auto* params = parseParamList();
    auto arrow = match(Arrow);
    auto* retType = arrow ? parseTypeSpec() : nullptr;
    if (facetState != FacetState::Type)
        if (auto* body = parseExpr())
            return allocate<ClosureFacet>(*fn, params,
                                          arrow.value_or(ErrorToken), retType,
                                          body);
    if (!params || !retType) {
        assert(false); // Invalid fn type
    }
    return allocate<FnTypeFacet>(*fn, params, arrow.value_or(ErrorToken),
                                 retType);
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
        lhs = allocate<BinaryFacet>(lhs, *tok, rhs);
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
        return allocate<BinaryFacet>(lhs, *tok, rhs);
    }
    return lhs;
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
        if (elem) {
            seq.push_back(std::move(elem));
            continue;
        }
        eat();
        //      assert(false); // Expected element
    }
}
