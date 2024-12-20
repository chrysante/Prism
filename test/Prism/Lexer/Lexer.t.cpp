#include <ostream>

#include <Prism/Lexer/Lexer.h>
#include <Prism/Lexer/LexicalIssue.h>
#include <Prism/Source/SourceContext.h>

#include <catch2/catch_test_macros.hpp>

using namespace prism;

namespace {

struct RefToken {
    SourceContext const& ctx;
    TokenKind kind;
    size_t line;
    size_t col;
    size_t len;
};

std::ostream& operator<<(std::ostream& str, RefToken const& tok) {
    return str << "{ kind: " << tok.kind << ", line: " << tok.line
               << ", col: " << tok.col << ", len: " << tok.len << " }";
}

bool operator==(RefToken const& ref, Token const& tok) {
    auto sourceLoc = ref.ctx.getSourceLocation(tok.index);
    return tok.kind == ref.kind && tok.sourceLen == ref.len &&
           sourceLoc.line == ref.line && sourceLoc.column == ref.col;
}

} // namespace

TEST_CASE("Integration", "[lexer]") {
    auto source = R"(
fn test(aParam: int, b: double) -> void {
    return "Hello World\n";
}
)";
    SourceContext ctx({}, source);
    IssueHandler H;
    Lexer L(source, H);
    CHECK(L.next() == RefToken{ ctx, TokenKind::Fn, 1, 0, 2 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 1, 3, 4 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::OpenParen, 1, 7, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 1, 8, 6 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Colon, 1, 14, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Int, 1, 16, 3 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Comma, 1, 19, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 1, 21, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Colon, 1, 22, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Double, 1, 24, 6 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::CloseParen, 1, 30, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Arrow, 1, 32, 2 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Void, 1, 35, 4 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::OpenBrace, 1, 40, 1 });

    CHECK(L.next() == RefToken{ ctx, TokenKind::Return, 2, 4, 6 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::StringLiteral, 2, 11, 15 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Semicolon, 2, 26, 1 });

    CHECK(L.next() == RefToken{ ctx, TokenKind::CloseBrace, 3, 0, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::End, 4, 0, 0 });

    CHECK(H.empty());
}

TEST_CASE("Integration no spaces", "[lexer]") {
    auto source = R"(
fn test(aParam:int,b:double)->void{return;})";
    IssueHandler H;
    Lexer L(source, H);
    CHECK(L.next().kind == TokenKind::Fn);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::OpenParen);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::Colon);
    CHECK(L.next().kind == TokenKind::Int);
    CHECK(L.next().kind == TokenKind::Comma);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::Colon);
    CHECK(L.next().kind == TokenKind::Double);
    CHECK(L.next().kind == TokenKind::CloseParen);
    CHECK(L.next().kind == TokenKind::Arrow);
    CHECK(L.next().kind == TokenKind::Void);
    CHECK(L.next().kind == TokenKind::OpenBrace);
    CHECK(L.next().kind == TokenKind::Return);
    CHECK(L.next().kind == TokenKind::Semicolon);
    CHECK(L.next().kind == TokenKind::CloseBrace);
    CHECK(L.next().kind == TokenKind::End);

    CHECK(H.empty());
}

static Token getSingle(std::string_view source) {
    IssueHandler H;
    Lexer L(source, H);
    auto tok = L.next();
    REQUIRE(L.next().kind == TokenKind::End);
    REQUIRE(H.empty());
    return tok;
}

TEST_CASE("Operators", "[lexer]") {
    SourceContext ctx({}, "        ");
    CHECK(getSingle("+") == RefToken{ ctx, TokenKind::Plus, 0, 0, 1 });
    CHECK(getSingle("-") == RefToken{ ctx, TokenKind::Minus, 0, 0, 1 });
    CHECK(getSingle("*") == RefToken{ ctx, TokenKind::Star, 0, 0, 1 });
    CHECK(getSingle("/") == RefToken{ ctx, TokenKind::Slash, 0, 0, 1 });

    CHECK(getSingle("+=") == RefToken{ ctx, TokenKind::PlusEq, 0, 0, 2 });
    CHECK(getSingle("-=") == RefToken{ ctx, TokenKind::MinusEq, 0, 0, 2 });
    CHECK(getSingle("*=") == RefToken{ ctx, TokenKind::StarEq, 0, 0, 2 });
    CHECK(getSingle("/=") == RefToken{ ctx, TokenKind::SlashEq, 0, 0, 2 });

    CHECK(getSingle("->") == RefToken{ ctx, TokenKind::Arrow, 0, 0, 2 });
}

TEST_CASE("Literals", "[lexer]") {
    SourceContext ctx({}, "        ");
    CHECK(getSingle(R"("\"")") ==
          RefToken{ ctx, TokenKind::StringLiteral, 0, 0, 4 });
    CHECK(getSingle(R"('\"')") ==
          RefToken{ ctx, TokenKind::CharLiteral, 0, 0, 4 });
    CHECK(getSingle("0b0101") ==
          RefToken{ ctx, TokenKind::IntLiteralBin, 0, 0, 6 });
    CHECK(getSingle("123") ==
          RefToken{ ctx, TokenKind::IntLiteralDec, 0, 0, 3 });
    CHECK(getSingle("0xabc") ==
          RefToken{ ctx, TokenKind::IntLiteralHex, 0, 0, 5 });
}

TEST_CASE("Errors", "[lexer]") {
    auto source = "#```abc`";
    SourceContext ctx({}, source);
    IssueHandler H;
    Lexer L(source, H);
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 0, 4, 3 });
    L.next();
    REQUIRE(H.size() == 2);
    auto errTok1 = dynamic_cast<LexicalIssue const&>(H.front()).token();
    CHECK(errTok1 == RefToken{ ctx, TokenKind::Error, 0, 0, 4 });
    auto errTok2 = dynamic_cast<LexicalIssue const&>(H.back()).token();
    CHECK(errTok2 == RefToken{ ctx, TokenKind::Error, 0, 7, 1 });
}
