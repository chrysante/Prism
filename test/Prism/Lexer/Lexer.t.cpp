#include <ostream>

#include <Prism/Lexer/Lexer.h>
#include <Prism/Lexer/LexicalDiagnostic.h>
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
fn test(aParam: int, b: f64) -> void {
    return "Hello World\n";
}
)";
    SourceContext ctx({}, source);
    auto DE = makeDefaultDiagnosticEmitter();
    Lexer L(ctx, *DE);
    CHECK(L.next() == RefToken{ ctx, TokenKind::Fn, 1, 0, 2 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 1, 3, 4 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::OpenParen, 1, 7, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 1, 8, 6 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Colon, 1, 14, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Int, 1, 16, 3 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Comma, 1, 19, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 1, 21, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Colon, 1, 22, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Float64, 1, 24, 3 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::CloseParen, 1, 27, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Arrow, 1, 29, 2 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Void, 1, 32, 4 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::OpenBrace, 1, 37, 1 });

    CHECK(L.next() == RefToken{ ctx, TokenKind::Return, 2, 4, 6 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::StringLiteral, 2, 11, 15 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::Semicolon, 2, 26, 1 });

    CHECK(L.next() == RefToken{ ctx, TokenKind::CloseBrace, 3, 0, 1 });
    CHECK(L.next() == RefToken{ ctx, TokenKind::End, 4, 0, 0 });

    CHECK(DE->getAll().empty());
}

TEST_CASE("Integration no spaces", "[lexer]") {
    auto source = R"(
fn test(aParam:int,b:f64)->void{return;})";
    SourceContext ctx({}, source);
    auto DE = makeDefaultDiagnosticEmitter();
    Lexer L(ctx, *DE);
    CHECK(L.next().kind == TokenKind::Fn);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::OpenParen);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::Colon);
    CHECK(L.next().kind == TokenKind::Int);
    CHECK(L.next().kind == TokenKind::Comma);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::Colon);
    CHECK(L.next().kind == TokenKind::Float64);
    CHECK(L.next().kind == TokenKind::CloseParen);
    CHECK(L.next().kind == TokenKind::Arrow);
    CHECK(L.next().kind == TokenKind::Void);
    CHECK(L.next().kind == TokenKind::OpenBrace);
    CHECK(L.next().kind == TokenKind::Return);
    CHECK(L.next().kind == TokenKind::Semicolon);
    CHECK(L.next().kind == TokenKind::CloseBrace);
    CHECK(L.next().kind == TokenKind::End);

    CHECK(DE->getAll().empty());
}

static Token getSingle(std::string_view source) {
    SourceContext ctx({}, source);
    auto DE = makeDefaultDiagnosticEmitter();
    Lexer L(ctx, *DE);
    auto tok = L.next();
    REQUIRE(L.next().kind == TokenKind::End);
    REQUIRE(DE->getAll().empty());
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

TEST_CASE("Unterminated string literals", "[lexer]") {
    auto source = R"(
"an unterminated string literal
id return
)";
    SourceContext ctx({}, source);
    auto DE = makeDefaultDiagnosticEmitter();
    Lexer L(ctx, *DE);
    CHECK(L.next().kind == TokenKind::StringLiteral);
    CHECK(L.next().kind == TokenKind::Identifier);
    CHECK(L.next().kind == TokenKind::Return);

    auto diags = DE->getAll();
    REQUIRE(diags.size() == 1);
    auto& err = dynamic_cast<LexicalDiagnostic const&>(*diags.front());
    CHECK(err.reason() == LexicalDiagnostic::UnterminatedStringLiteral);
}

TEST_CASE("Errors", "[lexer]") {
    auto source = "#```abc`";
    SourceContext ctx({}, source);
    auto DE = makeDefaultDiagnosticEmitter();
    Lexer L(ctx, *DE);
    CHECK(L.next() == RefToken{ ctx, TokenKind::Identifier, 0, 4, 3 });
    L.next();
    auto diags = DE->getAll();
    REQUIRE(diags.size() == 2);
    auto errRng1 =
        dynamic_cast<LexicalDiagnostic const&>(*diags.front()).sourceRange();
    CHECK(errRng1.value().slim() == SourceRange{ 0, 4 });
    auto errRng2 =
        dynamic_cast<LexicalDiagnostic const&>(*diags.back()).sourceRange();
    CHECK(errRng2.value().slim() == SourceRange{ 7, 1 });
}
