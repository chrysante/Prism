#ifndef PRISM_SOURCE_TOKEN_H
#define PRISM_SOURCE_TOKEN_H

#include <cstdint>

#include <Prism/Common/EnumUtil.h>

namespace prism {

/// List of different token types
enum class TokenKind : uint16_t {
#define TOKEN_KIND(Kind) Kind,
#include <Prism/Source/Token.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(TokenKind)

/// Tokens produced by the lexer and consumed by the parser. AST nodes also
/// carry tokens.
struct Token {
    /// The type of this token
    TokenKind kind;

    /// The number of characters in the source text
    uint16_t sourceLen;

    /// The index into the source text where this token appears
    uint32_t index;

    /// Global error token constant
    static Token const ErrorToken;
};

static_assert(sizeof(Token) == 8);

constexpr Token Token::ErrorToken = { .kind = TokenKind::Error };

} // namespace prism

#endif // PRISM_SOURCE_TOKEN_H
