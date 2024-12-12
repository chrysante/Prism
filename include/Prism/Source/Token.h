#ifndef PRISM_SOURCE_TOKEN_H
#define PRISM_SOURCE_TOKEN_H

#include <cstdint>
#include <iosfwd>

#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>

namespace prism {

/// List of different token types
enum class TokenKind : unsigned {
#define TOKEN_KIND(Kind) Kind,
#include <Prism/Source/Token.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(TokenKind)

static_assert(EnumCount<TokenKind> < 128, "We store the token kind in 7 bits");

/// Tokens produced by the lexer and consumed by the parser. AST nodes also
/// carry tokens.
struct Token {
    constexpr Token(TokenKind kind, uint32_t sourceLen, uint32_t index):
        kind(kind), sourceLen(sourceLen), index(index) {}

    /// Unused bits used as flags by the parse tree
    uint32_t unusedBits : 1 = 0;

    /// The type of this token
    TokenKind kind : 7 = {};

    /// The number of characters in the source text
    uint32_t sourceLen : 24 = 0;

    /// The index into the source text where this token appears
    uint32_t index = 0;

    /// Global error token constant
    static Token const ErrorToken;
};

static_assert(sizeof(Token) == 8);

constexpr Token Token::ErrorToken = { TokenKind::Error, 0, 0 };

std::ostream& operator<<(std::ostream& ostream, Token const& token);

} // namespace prism

#endif // PRISM_SOURCE_TOKEN_H
