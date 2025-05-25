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
    TraitImplDefFacet const* parseTraitDecl();
    TraitImplTypeFacet const* parseTraitTypeDecl();
    TraitImplFuncFacet const* parseTraitFuncDecl();
    VarDeclFacet const* parseVarDecl();
    PropertyDefFacet const* parsePropertyDef();
    PropertyImpl const* parsePropertyImpl();
    TypedefFacet const* parseTypedef();
    StmtFacet const* parseStmt();
    DeclFacet const* parseLocalDecl();
    ParamDeclFacet const* parseParamDecl();
    ParamDeclFacet const* parseThisParamDecl();
    TerminalFacet const* parseParamQualifier();
    TerminalFacet const* parsePassingConvention();
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
    Facet const* parseThisFacet();
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
                Var,   Let, Fn, Struct, Trait,      Impl,       Return,    For,
                While, Do,  If, Else,   CloseBrace, CloseBrace, Semicolon, End
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
                                              DiagnosticEmitter& DE) {
    Parser parser(alloc, sourceCtx, DE);
    return parser.parseSourceFile();
}

Facet const* prism::parseExpr(MonotonicBufferResource& alloc,
                              SourceContext const& sourceCtx,
                              DiagnosticEmitter& DE) {
    Parser parser(alloc, sourceCtx, DE);
    return parser.parseExpr();
}

Facet const* prism::parseTypeSpec(MonotonicBufferResource& alloc,
                                  SourceContext const& sourceCtx,
                                  DiagnosticEmitter& DE) {
    Parser parser(alloc, sourceCtx, DE);
    return parser.parseTypeSpec();
}

SourceFileFacet const* Parser::parseSourceFile() {
    return allocate<SourceFileFacet>(
        parseSequence(FN(parseGlobalDecl), Raise<ExpectedDecl>(), End));
}

DeclFacet const* Parser::parseGlobalDecl() {
    if (auto* fn = parseFuncDef()) return fn;
    if (auto* str = parseCompTypeDecl()) return str;
    if (auto* impl = parseTraitImpl()) return impl;
    if (auto* var = parseVarDecl()) return var;
    if (auto* type = parseTypedef()) return type;
    return nullptr;
}

FuncDeclFacet const* Parser::parseFuncDecl() {
    auto [declarator, genParams, name, params, arrow, retType] =
        makeParser()
            .fastFail(Match(Fn))
            .optRule({ FN(parseGenericParamList) })
            .rule({ FN(parseName), Raise<ExpectedDeclName>() })
            .rule({ FN(parseParamList), Raise<ExpectedParamList>() })
            .optRule({ Match(Arrow),
                       { FN(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
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
            .optRule({ FN(parseGenericParamList) })
            .rule({ FN(parseName), Raise<ExpectedDeclName>() })
            .rule({ FN(parseParamList), Raise<ExpectedParamList>() })
            .optRule({ Match(Arrow),
                       { FN(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .rule({ FN(parseFuncBody), Raise<ExpectedFuncBody>() })
            .eval();
    if (!declarator) return nullptr;
    return allocate<FuncDefFacet>(declarator, genParams, name, params, arrow,
                                  retType, body);
}

Facet const* Parser::parseFuncBody() { return parseCompoundFacet(); }

CompTypeDeclFacet const* Parser::parseCompTypeDecl() {
    auto [declarator, genParams, name, colon, baselist, openbrace, body,
          closebrace] = makeParser()
                            .fastFail(Match(Struct, Trait))
                            .optRule({ FN(parseGenericParamList) })
                            .rule({ FN(parseName), Raise<ExpectedDeclName>() })
                            .optRule({ Match(Colon), FN(parseBaseList) })
                            .rule(MatchExpect(OpenBrace))
                            .rule(FN(parseMemberList))
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
    auto elems = parseSequence(FN(parseBaseDecl), Raise<ExpectedBaseDecl>(),
                               OpenBrace, Comma);
    if (elems.empty()) raise<ExpectedBaseDecl>(peek());
    return allocate<BaseListFacet>(elems);
}

DeclFacet const* Parser::parseCompTypeMemberDecl() {
    if (auto* fn = parseFuncDef()) return fn;
    if (auto* str = parseCompTypeDecl()) return str;
    if (auto* var = parseVarDecl()) return var;
    if (auto* property = parsePropertyDef()) return property;
    if (auto* type = parseTypedef()) return type;
    return nullptr;
}

MemberListFacet const* Parser::parseMemberList() {
    auto elems = parseSequence(FN(parseCompTypeMemberDecl),
                               Raise<ExpectedDecl>(), CloseBrace);
    return allocate<MemberListFacet>(elems);
}

TraitImplFacet const* Parser::parseTraitImpl() {
    auto [declarator, genParams, decl] =
        makeParser()
            .fastFail(Match(Impl))
            .optRule({ FN(parseGenericParamList) })
            .rule({ FN(parseTraitDecl), Raise<ExpectedTraitDecl>() })
            .eval();
    if (!declarator) return nullptr;
    return allocate<TraitImplFacet>(declarator, genParams, decl);
}

TraitImplDefFacet const* Parser::parseTraitDecl() {
    if (auto* func = parseTraitFuncDecl()) return func;
    if (auto* type = parseTraitTypeDecl()) return type;
    return nullptr;
}

TraitImplTypeFacet const* Parser::parseTraitTypeDecl() {
    auto [trait, forTok, conforming, openbrace, body, closebrace] =
        makeParser()
            .fastFail(FN(parseTypeSpec))
            .rule(MatchExpect(For))
            .rule({ FN(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .rule(MatchExpect(OpenBrace))
            .rule(FN(parseMemberList))
            .rule(MatchExpect(CloseBrace))
            .eval();
    if (!trait) return nullptr;
    return allocate<TraitImplTypeFacet>(trait, forTok, conforming, openbrace,
                                        body, closebrace);
}

TraitImplFuncFacet const* Parser::parseTraitFuncDecl() {
    auto [func, forTok, conforming, body] =
        makeParser()
            .fastFail(FN(parseFuncDecl))
            .rule(MatchExpect(For))
            .rule({ FN(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .rule({ FN(parseFuncBody), Raise<ExpectedFuncBody>() })
            .eval();
    if (!func) return nullptr;
    return allocate<TraitImplFuncFacet>(func, forTok, conforming, body);
}

VarDeclFacet const* Parser::parseVarDecl() {
    auto [declarator, name, colon, type, assign, init, semicolon] =
        makeParser()
            .fastFail(Match(Var, Let))
            .rule(FN(parseName))
            .optRule({ Match(Colon),
                       { FN(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .optRule({ Match(Equal), { FN(parseExpr), Raise<ExpectedExpr>() } })
            .rule(MatchExpect(Semicolon))
            .eval();
    if (!declarator) return nullptr;
    return allocate<VarDeclFacet>(declarator, name, colon, type, assign, init,
                                  semicolon);
}

PropertyDefFacet const* Parser::parsePropertyDef() {
    auto parsePropertyName = [this]() -> Facet const* {
        if (auto nameTok = match(Identifier)) return toTerminal(*nameTok);
        auto openBracket = peekMatch(OpenBracket, 1);
        auto closeBracket = peekMatch(CloseBracket, 2);
        if (openBracket && closeBracket) {
            eat(2);
            return allocate<ListFacet>(std::span<Facet const* const>(
                { toTerminal(*openBracket), toTerminal(*closeBracket) }));
        }
        return nullptr;
    };
    auto parseImplList = [this] {
        auto seq = parseSequence(FN(parsePropertyImpl), Raise<ExpectedId>(),
                                 CloseBrace);
        return allocate<PropertyImplListFacet>(std::move(seq));
    };
    auto [declarator, name, params, arrow, typespec, openBrace, implList,
          closeBrace] = makeParser()
                            .fastFail(Match(Property))
                            .rule({ parsePropertyName, Raise<ExpectedId>() })
                            .rule(FN(parseParamList))
                            .optRule({ Match(Arrow), FN(parseTypeSpec) })
                            .rule(MatchExpect(OpenBrace))
                            .rule(parseImplList)
                            .rule(MatchExpect(CloseBrace))
                            .eval();
    if (!declarator) return nullptr;
    return allocate<PropertyDefFacet>(declarator, name, params, arrow, typespec,
                                      openBrace, implList, closeBrace);
}

PropertyImpl const* Parser::parsePropertyImpl() {
    auto [name, params, body] = makeParser()
                                    .fastFail(Match(Identifier))
                                    .rule(FN(parseParamList))
                                    .rule(FN(parseCompoundFacet))
                                    .eval();
    if (!name) return nullptr;
    return allocate<PropertyImpl>(name, params, body);
}

TypedefFacet const* Parser::parseTypedef() {
    auto [declarator, name, colon, traitBound, assign, def, semicolon] =
        makeParser()
            .fastFail(Match(Typedef))
            .rule(FN(parseUnqualName))
            .optRule({ Match(Colon),
                       { FN(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .optRule({ Match(Equal),
                       { FN(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .rule(MatchExpect(Semicolon))
            .eval();
    if (!declarator) return nullptr;
    return allocate<TypedefFacet>(declarator, name, colon, traitBound, assign,
                                  def, semicolon);
}

StmtFacet const* Parser::parseStmt() {
    if (auto* decl = parseLocalDecl()) return decl;
    if (auto* stmt = parseReturnStmt()) return stmt;
    if (auto* stmt = parseExprStmt()) return stmt;
    if (auto* stmt = parseEmptyStmt()) return stmt;
    return nullptr;
}

DeclFacet const* Parser::parseLocalDecl() {
    if (auto* var = parseVarDecl()) return var;
    if (auto* type = parseTypedef()) return type;
    return nullptr;
}

ParamDeclFacet const* Parser::parseParamDecl() {
    auto parseParamType = [this] {
        return makeParser()
            .rule(FN(parseParamQualifier))
            .rule(FN(parsePassingConvention))
            .rule({ FN(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .eval();
    };
    auto nameTok = peekMatch(Identifier, 1);
    auto colonTok = peekMatch(Colon, 2);
    if (nameTok && colonTok) {
        eat(2);
        auto* name = allocate<TerminalFacet>(*nameTok);
        auto* colon = allocate<TerminalFacet>(*colonTok);
        auto [qualifier, passingConv, type] = parseParamType();
        return allocate<NamedParamDeclFacet>(name, colon, qualifier,
                                             passingConv, type);
    }
    if (auto* This = parseThisParamDecl()) return This;
    auto [qualifier, passingConv, type] = parseParamType();
    return allocate<NamedParamDeclFacet>(nullptr, nullptr, qualifier,
                                         passingConv, type);
}

static constexpr std::array PassingConventionTokens = { In, Inout, Sink };

ParamDeclFacet const* Parser::parseThisParamDecl() {
    size_t offset = 1;
    auto passingConvTok = peekMatch(PassingConventionTokens, offset);
    if (passingConvTok) ++offset;
    auto dynTok = peekMatch(Dyn, offset);
    if (dynTok) ++offset;
    auto thisTok = peekMatch(This, offset);
    if (!thisTok) return nullptr;
    eat(offset);
    return allocate<ThisParamDeclFacet>(toTerminal(passingConvTok),
                                        toTerminal(dynTok),
                                        toTerminal(thisTok));
}

TerminalFacet const* Parser::parseParamQualifier() {
    return toTerminal(match(Aliasing));
}

TerminalFacet const* Parser::parsePassingConvention() {
    return toTerminal(match(PassingConventionTokens));
}

ParamListFacet const* Parser::parseParamList() {
    if (!match(OpenParen)) return nullptr;
    auto seq = parseSequence(FN(parseParamDecl), Raise<ExpectedParamDecl>(),
                             CloseParen, Comma);
    match(CloseParen);
    return allocate<ParamListFacet>(seq);
}

GenParamDeclFacet const* Parser::parseGenericParamDecl() {
    auto [name, colon, type] =
        makeParser()
            .rule({ FN(parseUnqualName), Raise<ExpectedDeclName>() })
            .rule(MatchExpect(Colon))
            .rule({ FN(parseTypeSpec), Raise<ExpectedTypeSpec>() })
            .eval();
    return allocate<GenParamDeclFacet>(name, colon, type);
}

GenParamListFacet const* Parser::parseGenericParamList() {
    if (!match(OpenBracket)) return nullptr;
    auto seq = parseSequence(FN(parseGenericParamDecl),
                             Raise<ExpectedParamDecl>(), CloseBracket, Comma);
    match(CloseBracket);
    return allocate<GenParamListFacet>(seq);
}

ReturnStmtFacet const* Parser::parseReturnStmt() {
    auto [ret, expr, semicolon] = makeParser()
                                      .fastFail(Match(Return))
                                      .optRule({ FN(parseExpr) })
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
                                 .fastFail(FN(parseExpr))
                                 .rule(MatchExpect(Semicolon))
                                 .eval();
    if (!expr) return nullptr;
    return allocate<ExprStmtFacet>(expr, semicolon);
}

Facet const* Parser::parseFacet() { return parseAssignFacet(); }

Facet const* Parser::parseExpr() {
    return withFacetState(FacetState::Expression, FN(parseFacet));
}

Facet const* Parser::parseTypeSpec() {
    return withFacetState(FacetState::Type, [this] {
        return parseBinaryFacetLTR(Ampersand, FN(parsePrefixFacet));
    });
}

Facet const* Parser::parseAssignFacet() {
    static constexpr TokenKind Ops[] = {
        Equal,       PlusEq,    MinusEq,           StarEq,
        SlashEq,     PercentEq, DoubleLeftAngleEq, DoubleRightAngleEq,
        AmpersandEq, VertBarEq, CircumflexEq
    };
    return parseBinaryFacetRTL(Ops, FN(parseTernCondFacet));
}

Facet const* Parser::parseTernCondFacet() {
    auto* cond = parseBinCondFacet();
    if (!cond) return nullptr;
    auto [question, lhs, colon, rhs] =
        makeParser()
            .fastFail(Match(Question))
            .rule({ FN(parseAssignFacet), Raise<ExpectedExpr>() })
            .rule(MatchExpect(Colon))
            .rule({ FN(parseTernCondFacet), Raise<ExpectedExpr>() })
            .eval();
    if (!question) return cond;
    return allocate<CondFacet>(cond, question, lhs, colon, rhs);
}

Facet const* Parser::parseBinCondFacet() {
    return parseBinaryFacetRTL(QuestionColon, FN(parseLogicalOrFacet));
}

Facet const* Parser::parseLogicalOrFacet() {
    return parseBinaryFacetLTR(DoubleVertBar, FN(parseLogicalAndFacet));
}

Facet const* Parser::parseLogicalAndFacet() {
    return parseBinaryFacetLTR(DoubleAmpersand, FN(parseOrFacet));
}

Facet const* Parser::parseOrFacet() {
    return parseBinaryFacetLTR(VertBar, FN(parseXorFacet));
}

Facet const* Parser::parseXorFacet() {
    return parseBinaryFacetLTR(Circumflex, FN(parseAndFacet));
}

Facet const* Parser::parseAndFacet() {
    return parseBinaryFacetLTR(Ampersand, FN(parseEqFacet));
}

Facet const* Parser::parseEqFacet() {
    return parseBinaryFacetLTR({ DoubleEqual, NotEq }, FN(parseRelFacet));
}

Facet const* Parser::parseRelFacet() {
    return parseBinaryFacetLTR({ LeftAngle, LeftAngleEq, RightAngle,
                                 RightAngleEq },
                               FN(parseShiftFacet));
}

Facet const* Parser::parseShiftFacet() {
    return parseBinaryFacetLTR({ DoubleLeftAngle, DoubleRightAngle },
                               FN(parseAddFacet));
}

Facet const* Parser::parseAddFacet() {
    return parseBinaryFacetLTR({ Plus, Minus }, FN(parseMulFacet));
}

Facet const* Parser::parseMulFacet() {
    return parseBinaryFacetLTR({ Star, Slash, Percent }, FN(parseCastFacet));
}

Facet const* Parser::parseCastFacet() {
    Facet const* facet = parsePrefixFacet();
    if (!facet) return nullptr;
    while (true) {
        auto [as, type] =
            makeParser()
                .fastFail(Match(As))
                .rule({ FN(parseTypeSpec), Raise<ExpectedTypeSpec>() })
                .eval();
        if (!as) return facet;
        facet = allocate<BinaryFacet>(facet, as, type);
    }
}

Facet const* Parser::parsePrefixFacet() {
    static constexpr std::array QualOps = { Exclam, Mut, Star, Ampersand,
                                            Question };
    static constexpr std::array Ops = { Plus,      Minus,      Tilde,
                                        Exclam,    DoublePlus, DoubleMinus,
                                        Mut,       Dyn,        Star,
                                        Ampersand, Question,   New };
    auto* operation = Match(Ops)();
    if (!operation) return parsePostfixFacet();
    if (facetState == FacetState::Type &&
        ranges::contains(QualOps, operation->token().kind))
    {
        auto [operand] =
            makeParser()
                .rule({ FN(parsePrefixFacet), Raise<ExpectedExpr>() })
                .eval();
        if (operand) return allocate<PrefixFacet>(operation, operand);
        return operation;
    }
    auto [operand] = makeParser()
                         .rule({ FN(parsePrefixFacet), Raise<ExpectedExpr>() })
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
                .rule(FN0(&, parseListFacet(Comma, closeKind)))
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
        auto [period, member] =
            makeParser()
                .fastFail(Match(Period))
                .rule({ FN(parseUnqualName), Raise<ExpectedId>() })
                .eval();
        if (!period) return base == primary ? nullptr : base;
        base = allocate<BinaryFacet>(base, period, member);
    }
}

Facet const* Parser::parsePrimaryFacet() {
    static constexpr std::array LiteralKinds = {
#define LITERAL_TOKEN_KIND(Name, ...)         Name,
#define KEYWORD_LITERAL_TOKEN_KIND(Name, ...) Name,
#include "Prism/Source/Token.def"
    };
    static constexpr std::array TypeKinds = {
#define TYPE_TOKEN_KIND(Name, ...) Name,
#include "Prism/Source/Token.def"
        Identifier
    };

    if (auto* thisFacet = parseThisFacet()) return thisFacet;
    if (auto tok = match(TypeKinds)) return toTerminal(*tok);
    if (auto* closure = parseClosureOrFnTypeFacet()) return closure;
    if (facetState != FacetState::Type) {
        if (auto tok = match(LiteralKinds)) return toTerminal(*tok);
        if (auto* facet = parseParenthesisedFacet()) return facet;
        if (auto* array = parseArrayFacet()) return array;
        if (auto* cmpFacet = parseCompoundFacet()) return cmpFacet;
        if (auto* autoArg = parseAutoArgFacet()) return autoArg;
    }
    return nullptr;
}

Facet const* Parser::parseThisFacet() {
    auto [thisTerm, typeTerm] =
        makeParser().fastFail(Match(This)).fastFail(Match(Type)).eval();
    if (!thisTerm) return nullptr;
    if (!typeTerm) {
        if (facetState == FacetState::Type)
            return nullptr;
        else
            return thisTerm;
    }
    return allocate<PrefixFacet>(thisTerm, typeTerm);
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
    auto [intro, name, colon, passingConv, type] =
        makeParser()
            .fastFail(Match(AutoArgIntro))
            .optRule({ FN(parseUnqualName) })
            .optRule({ Match(Colon),
                       FN(parsePassingConvention),
                       { FN(parseTypeSpec), Raise<ExpectedTypeSpec>() } })
            .eval();
    if (!intro) return nullptr;
    return allocate<AutoArgFacet>(intro, name, colon, passingConv, type);
}

Facet const* Parser::parseClosureOrFnTypeFacet() {
    auto bodyIfNotType = [this] {
        return facetState != FacetState::Type ? parseExpr() : nullptr;
    };
    auto [fn, params, arrow, retType, body] =
        makeParser()
            .fastFail(Match(Fn))
            .optRule({ FN(parseParamList) })
            .optRule({ Match(Arrow), FN(parseTypeSpec) })
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
            .rule({ FN(parseExpr), Raise<ExpectedExpr>() })
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
        parseSequence(FN(parseExpr), Raise<ExpectedExpr>(), end, delim);
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
            .rule({ FN0(&, parseBinaryFacetRTL(acceptedOperators, next)),
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
