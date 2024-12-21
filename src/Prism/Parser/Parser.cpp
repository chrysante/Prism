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

template <typename>
struct TagT {};

template <typename T>
constexpr TagT<T> Tag{};

enum class FacetState { Expression, Type };

struct Parser: LinearParser {
    using LinearParser::LinearParser;

    SourceFileFacet const* parseSourceFile();
    DeclFacet const* parseGlobalDecl();
    FuncDeclFacet const* parseFuncDecl();
    FuncDefFacet const* parseFuncDef();
    Facet const* parseFuncBody();
    CompTypeDeclFacet const* parseCompTypeDecl();
    BaseDeclFacet const* parseBaseDecl();
    BaseListFacet const* parseBaseList();
    DeclFacet const* parseCompTypeMemberDecl();
    MemberListFacet const* parseMemberList();
    TraitImplFacet const* parseTraitImpl();
    TraitImplDeclFacet const* parseTraitDecl();
    TraitTypeDeclFacet const* parseTraitTypeDecl();
    TraitFuncDeclFacet const* parseTraitFuncDecl();
    VarDeclFacet const* parseVarDecl();
    StmtFacet const* parseStmt();
    DeclFacet const* parseLocalDecl();
    ParamDeclFacet const* parseParamDecl();
    ParamDeclFacet const* parseThisParamDecl();
    ParamListFacet const* parseParamList();
    GenParamDeclFacet const* parseGenericParamDecl();
    GenParamListFacet const* parseGenericParamList();
    ReturnStmtFacet const* parseReturnStmt();
    EmptyStmtFacet const* parseEmptyStmt();
    ExprStmtFacet const* parseExprStmt();

    Facet const* parseName();
    Facet const* parseUnqualName();
    Facet const* parseFacet();
    Facet const* parseExpr();
    Facet const* parseTypeSpec();

    Facet const* parseAssignFacet();
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
    Facet const* parseCastFacet();
    Facet const* parsePrefixFacet();
    Facet const* parsePostfixFacet();
    Facet const* parseCallFacet(Facet const* primary);
    Facet const* parseMemAccessFacet(Facet const* primary);
    Facet const* parsePrimaryFacet();
    Facet const* parseFstringFacet();
    CompoundFacet const* parseCompoundFacet();
    Facet const* parseAutoArgFacet();
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
                Var, Let, Fn,   Struct,     Trait, Impl,      Return,    For, While,
                Do,  If,  Else, CloseBrace, CloseBrace, Semicolon, End
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

    FacetState facetState = FacetState::Expression;
};

} // namespace

SourceFileFacet const* prism::parseSourceFile(MonotonicBufferResource& alloc,
                                              SourceContext const& sourceCtx,
                                              IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseSourceFile();
}

Facet const* prism::parseExpr(MonotonicBufferResource& alloc,
                              SourceContext const& sourceCtx,
                              IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseExpr();
}

Facet const* prism::parseTypeSpec(MonotonicBufferResource& alloc,
                                  SourceContext const& sourceCtx,
                                  IssueHandler& iss) {
    Parser parser(alloc, sourceCtx, iss);
    return parser.parseTypeSpec();
}

#define fn(Name, ...)    [this] { return Name(__VA_ARGS__); }
#define valfn(Name, ...) [=, this] { return Name(__VA_ARGS__); }

SourceFileFacet const* Parser::parseSourceFile() {
    return allocate<SourceFileFacet>(
        parseSequence(fn(parseGlobalDecl), Raise<ExpectedDecl>(), End));
}

DeclFacet const* Parser::parseGlobalDecl() {
    if (auto fn = parseFuncDef()) return fn;
    if (auto str = parseCompTypeDecl()) return str;
    if (auto var = parseVarDecl()) return var;
    if (auto impl = parseTraitImpl()) return impl;
    return nullptr;
}

FuncDeclFacet const* Parser::parseFuncDecl() {
    auto [declarator, genParams, name, params, arrow, retType] =
        makeParser()
            .fastFail(Match(Fn))
            .optRule({ fn(parseGenericParamList) })
            .rule({ fn(parseName), Raise<ExpectedDeclName>() })
            .rule({ fn(parseParamList), Raise<ExpectedParamList>() })
            .optRule({ Match(Arrow),
                       { fn(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .eval();
    if (!declarator) return nullptr;
    return allocate<FuncDeclFacet>(declarator, genParams, name, params, arrow,
                                   retType);
}

FuncDefFacet const* Parser::parseFuncDef() {
    // We can't abstract the construction of the parser into a function, because
    // the rules are captured by reference. We leave this code duplication until
    // we rewrite the parser generator.
    auto [declarator, genParams, name, params, arrow, retType, body] =
        makeParser()
            .fastFail(Match(Fn))
            .optRule({ fn(parseGenericParamList) })
            .rule({ fn(parseName), Raise<ExpectedDeclName>() })
            .rule({ fn(parseParamList), Raise<ExpectedParamList>() })
            .optRule({ Match(Arrow),
                       { fn(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .rule({ fn(parseFuncBody), Raise<ExpectedFuncBody>() })
            .eval();
    if (!declarator) return nullptr;
    return allocate<FuncDefFacet>(declarator, genParams, name, params, arrow,
                                  retType, body);
}

Facet const* Parser::parseFuncBody() {
    if (auto semicolon = match(Semicolon))
        return allocate<TerminalFacet>(*semicolon);
    return parseCompoundFacet();
}

CompTypeDeclFacet const* Parser::parseCompTypeDecl() {
    auto [declarator, genParams, name, colon, baselist, openbrace, body,
          closebrace] = makeParser()
                            .fastFail(Match(Struct, Trait))
                            .optRule({ fn(parseGenericParamList) })
                            .rule({ fn(parseName), Raise<ExpectedDeclName>() })
                            .optRule({ Match(Colon), fn(parseBaseList) })
                            .rule(MatchExpect(OpenBrace))
                            .rule(fn(parseMemberList))
                            .rule(MatchExpect(CloseBrace))
                            .eval();
    if (!declarator) return nullptr;
    return allocate<CompTypeDeclFacet>(declarator, genParams, name, colon,
                                       baselist, openbrace, body, closebrace);
}

BaseDeclFacet const* Parser::parseBaseDecl() {
    auto* type = parseTypeSpec();
    return allocate<BaseDeclFacet>(nullptr, type);
}

BaseListFacet const* Parser::parseBaseList() {
    auto elems = parseSequence(fn(parseBaseDecl), Raise<ExpectedBaseDecl>(),
                               OpenBrace, Comma);
    if (elems.empty()) raise<ExpectedBaseDecl>(peek());
    return allocate<BaseListFacet>(elems);
}

DeclFacet const* Parser::parseCompTypeMemberDecl() {
    if (auto fn = parseFuncDef()) return fn;
    if (auto str = parseCompTypeDecl()) return str;
    if (auto var = parseVarDecl()) return var;
    return nullptr;
}

MemberListFacet const* Parser::parseMemberList() {
    auto elems = parseSequence(fn(parseCompTypeMemberDecl),
                               Raise<ExpectedDecl>(), CloseBrace);
    return allocate<MemberListFacet>(elems);
}

TraitImplFacet const* Parser::parseTraitImpl() {
    auto [declarator, genParams, decl] =
        makeParser()
            .fastFail(Match(Impl))
            .optRule({ fn(parseGenericParamList) })
            .rule({ fn(parseTraitDecl), Raise<ExpectedTraitDecl>() })
            .eval();
    if (!declarator) return nullptr;
    return allocate<TraitImplFacet>(declarator, genParams, decl);
}

TraitImplDeclFacet const* Parser::parseTraitDecl() {
    if (auto* func = parseTraitFuncDecl()) return func;
    if (auto* type = parseTraitTypeDecl()) return type;
    return nullptr;
}

TraitTypeDeclFacet const* Parser::parseTraitTypeDecl() {
    auto [trait, forTok, conforming, openbrace, body, closebrace] =
        makeParser()
            .fastFail(fn(parseTypeSpec))
            .rule(MatchExpect(For))
            .rule({ fn(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .rule(MatchExpect(OpenBrace))
            .rule(fn(parseMemberList))
            .rule(MatchExpect(CloseBrace))
            .eval();
    if (!trait) return nullptr;
    return allocate<TraitTypeDeclFacet>(trait, forTok, conforming, openbrace,
                                        body, closebrace);
}

TraitFuncDeclFacet const* Parser::parseTraitFuncDecl() {
    auto [func, forTok, conforming, body] =
        makeParser()
            .fastFail(fn(parseFuncDecl))
            .rule(MatchExpect(For))
            .rule({ fn(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .rule({ fn(parseFuncBody), Raise<ExpectedFuncBody>() })
            .eval();
    if (!func) return nullptr;
    return allocate<TraitFuncDeclFacet>(func, forTok, conforming, body);
}

VarDeclFacet const* Parser::parseVarDecl() {
    auto [declarator, name, colon, type, assign, init, semicolon] =
        makeParser()
            .fastFail(Match(Var, Let))
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
    if (auto* This = parseThisParamDecl()) return This;
    auto [name, colon, type] =
        makeParser()
            .rule({ fn(parseUnqualName), Raise<ExpectedDeclName>() })
            .rule(MatchExpect(Colon))
            .rule({ fn(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .eval();
    return allocate<NamedParamDeclFacet>(name, colon, type);
}

ParamDeclFacet const* Parser::parseThisParamDecl() {
    auto primary = [&](auto& primary) -> Facet const* {
        if (auto tok = match(This)) return allocate<TerminalFacet>(*tok);
        return nullptr;
    };
    auto prefix = [&](auto& prefix) -> Facet const* {
        auto tok = match({ Ampersand, Mut, Dyn });
        if (!tok) return primary(primary);
        auto [operand] =
            makeParser()
                .rule({ valfn(prefix, prefix), Raise<ExpectedTypeSpec>() })
                .eval();
        return allocate<PrefixFacet>(*tok, operand);
    };
    if (auto* spec = prefix(prefix)) return allocate<ThisParamDeclFacet>(spec);
    return nullptr;
}

ParamListFacet const* Parser::parseParamList() {
    if (!match(OpenParen)) return nullptr;
    auto seq = parseSequence(fn(parseParamDecl), Raise<ExpectedParamDecl>(),
                             CloseParen, Comma);
    match(CloseParen);
    return allocate<ParamListFacet>(seq);
}

GenParamDeclFacet const* Parser::parseGenericParamDecl() {
    auto [name, colon, type] =
        makeParser()
            .rule({ fn(parseUnqualName), Raise<ExpectedDeclName>() })
            .rule(MatchExpect(Colon))
            .rule({ fn(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .eval();
    return allocate<GenParamDeclFacet>(name, colon, type);
}

GenParamListFacet const* Parser::parseGenericParamList() {
    if (!match(OpenBracket)) return nullptr;
    auto seq = parseSequence(fn(parseGenericParamDecl),
                             Raise<ExpectedParamDecl>(), CloseBracket, Comma);
    match(CloseBracket);
    return allocate<GenParamListFacet>(seq);
}

ReturnStmtFacet const* Parser::parseReturnStmt() {
    auto [ret, expr, semicolon] = makeParser()
                                      .fastFail(Match(Return))
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
    auto [expr, semicolon] = makeParser()
                                 .fastFail(fn(parseExpr))
                                 .rule(MatchExpect(Semicolon))
                                 .eval();
    if (!expr) return nullptr;
    return allocate<ExprStmtFacet>(expr, semicolon);
}

Facet const* Parser::parseFacet() { return parseAssignFacet(); }

Facet const* Parser::parseExpr() {
    return withFacetState(FacetState::Expression, fn(parseFacet));
}

Facet const* Parser::parseTypeSpec() {
    return withFacetState(FacetState::Type, fn(parsePrefixFacet));
}

Facet const* Parser::parseAssignFacet() {
    static constexpr TokenKind Ops[] = {
        Equal,       PlusEq,    MinusEq,           StarEq,
        SlashEq,     PercentEq, DoubleLeftAngleEq, DoubleRightAngleEq,
        AmpersandEq, VertBarEq, CircumflexEq
    };
    return parseBinaryFacetRTL(Ops, fn(parseTernCondFacet));
}

Facet const* Parser::parseTernCondFacet() {
    auto* cond = parseBinCondFacet();
    if (!cond) return nullptr;
    auto [question, lhs, colon, rhs] =
        makeParser()
            .fastFail(Match(Question))
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
    return parseBinaryFacetLTR({ Star, Slash, Percent }, fn(parseCastFacet));
}

Facet const* Parser::parseCastFacet() {
    Facet const* facet = parsePrefixFacet();
    if (!facet) return nullptr;
    while (true) {
        auto [as, type] =
            makeParser()
                .fastFail(Match(As))
                .rule({ fn(parseTypeSpec), Raise<ExpectedTypeSpec>() })
                .eval();
        if (!as) return facet;
        facet = allocate<BinaryFacet>(facet, as, type);
    }
}

Facet const* Parser::parsePrefixFacet() {
    static constexpr std::array QualOps = { Exclam, Mut, Star, Ampersand,
                                            Question };
    static constexpr std::array Ops = {
        Plus, Minus, Tilde,     Exclam,   DoublePlus, DoubleMinus, Mut,
        Dyn,  Star,  Ampersand, Question, New,        Move
    };
    auto* operation = Match(Ops)();
    if (!operation) return parsePostfixFacet();
    if (facetState == FacetState::Type &&
        ranges::contains(QualOps, operation->token().kind))
    {
        auto [operand] =
            makeParser()
                .fastFail({ fn(parsePrefixFacet), Raise<ExpectedExpr>() })
                .eval();
        if (operand) return allocate<PrefixFacet>(operation, operand);
        return operation;
    }
    auto [operand] = makeParser()
                         .rule({ fn(parsePrefixFacet), Raise<ExpectedExpr>() })
                         .eval();
    return allocate<PrefixFacet>(operation, operand);
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
        if (auto* memacc = parseMemAccessFacet(operand)) {
            operand = memacc;
            continue;
        }
        return operand;
    }
}

Facet const* Parser::parseCallFacet(Facet const* primary) {
    if (isa<CompoundFacet>(primary)) return nullptr;
    auto impl = [&]<typename T>(TagT<T>, TokenKind openKind,
                                TokenKind closeKind) -> CallBaseFacet const* {
        auto [open, args, close] =
            makeParser()
                .fastFail(Match(openKind))
                .rule(valfn(parseListFacet, Comma, closeKind))
                .rule(MatchExpect(closeKind))
                .eval();
        if (!open) return nullptr;
        return allocate<T>(primary, open, args, close);
    };
    if (auto* call = impl(Tag<CallFacet>, OpenParen, CloseParen)) return call;
    if (auto* call = impl(Tag<IndexFacet>, OpenBracket, CloseBracket))
        return call;
    if (facetState != FacetState::Type)
        if (auto* call = impl(Tag<AggrConstructFacet>, OpenBrace, CloseBrace))
            return call;
    return nullptr;
}

Facet const* Parser::parseMemAccessFacet(Facet const* primary) {
    auto* base = primary;
    while (true) {
        auto [period, member] = makeParser()
            .fastFail(Match(Period))
            .rule({ fn(parseUnqualName), Raise<ExpectedId>() })
            .eval();
        if (!period) return base == primary ? nullptr : base;
        base = allocate<BinaryFacet>(base, period, member);
    }
}

Facet const* Parser::parsePrimaryFacet() {
    static constexpr std::array TermKinds = {
#define PRIMARY_TOKEN_KIND(Name, ...) Name,
#define LITERAL_TOKEN_KIND(Name, ...) Name,
#include "Prism/Source/Token.def"
        Identifier
    };
    if (auto tok = match(TermKinds)) return allocate<TerminalFacet>(*tok);
    if (auto* closure = parseClosureOrFnTypeFacet()) return closure;
    if (facetState != FacetState::Type) {
        if (auto* facet = parseParenthesisedFacet()) return facet;
        if (auto* array = parseArrayFacet()) return array;
        if (auto* cmpFacet = parseCompoundFacet()) return cmpFacet;
        if (auto* autoArg = parseAutoArgFacet()) return autoArg;
    }
    return nullptr;
}

CompoundFacet const* Parser::parseCompoundFacet() {
    auto open = match(OpenBrace);
    if (!open) return nullptr;
    utl::small_vector<StmtFacet const*> elems;
    auto* returnFacet = EVAL_AS(Facet const*) {
        while (true) {
            Facet const* expr = parseExpr();
            if (expr) {
                if (peekMatch(CloseBrace)) return expr;
                std::optional<Token> semicolon;
                if (!isa<CompoundFacet>(expr)) {
                    semicolon = matchExpect(Semicolon);
                }
                auto* stmt = allocate<ExprStmtFacet>(expr, semicolon.value_or(
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

Facet const* Parser::parseAutoArgFacet() {
    auto [intro, name, colon, type] =
        makeParser()
            .fastFail(Match(AutoArgIntro))
            .optRule({ fn(parseUnqualName) })
            .optRule({ Match(Colon),
                       { fn(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .eval();
    if (!intro) return nullptr;
    return allocate<AutoArgFacet>(intro, name, colon, type);
}

Facet const* Parser::parseClosureOrFnTypeFacet() {
    auto bodyIfNotType = [this] {
        return facetState != FacetState::Type ? parseExpr() : nullptr;
    };
    auto [fn, params, arrow, retType, body] =
        makeParser()
            .fastFail(Match(Fn))
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
            .fastFail(Match(OpenParen))
            .rule({ fn(parseExpr), Raise<ExpectedExpr>() })
            .rule(Match(CloseParen))
            .eval();
    if (!open) return nullptr;
    return allocate<ParenthesisedFacet>(open, facet, close);
}

Facet const* Parser::parseArrayFacet() {
    auto listParser = [this] { return parseListFacet(Comma, CloseBracket); };
    auto [open, list, close] = makeParser()
                                   .fastFail(Match(OpenBracket))
                                   .rule(listParser)
                                   .rule(Match(CloseBracket))
                                   .eval();
    if (!open) return nullptr;
    return allocate<ArrayFacet>(open, list, close);
}

Facet const* Parser::parseListFacet(TokenKind delim, TokenKind end) {
    auto argList =
        parseSequence(fn(parseExpr), Raise<ExpectedExpr>(), end, delim);
    return allocate<ListFacet>(argList);
}

Facet const* Parser::parseFstringFacet() { return nullptr; }

Facet const* Parser::parseBinaryFacetLTR(
    VolatileList<TokenKind const> acceptedOperators, ParserFn auto next) {
    auto* lhs = next();
    if (!lhs) return nullptr;
    while (true) {
        auto [op, rhs] = makeParser()
                             .fastFail(Match(acceptedOperators))
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
            .fastFail(Match(acceptedOperators))
            .rule({ valfn(parseBinaryFacetRTL, acceptedOperators, next),
                    Raise<ExpectedExpr>() })
            .eval();
    if (!op) return lhs;
    return allocate<BinaryFacet>(lhs, op, rhs);
}

Facet const* Parser::parseName() {
    auto* base = parseUnqualName();
    if (!base) return nullptr;
    if (auto* memacc = parseMemAccessFacet(base)) return memacc;
    return base;
}

Facet const* Parser::parseUnqualName() {
    return makeParser().fastFail(Match(Identifier)).eval()[0];
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
