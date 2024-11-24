#ifndef PRISM_SOURCE_TOKEN_H
#define PRISM_SOURCE_TOKEN_H

#include <cstdint>

#include <Prism/Common/EnumUtil.h>

namespace prism {

///
enum class TokenKind : uint16_t {
#define TOKEN_KIND(Kind) Kind,
#include <Prism/Source/Token.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(TokenKind)

///
struct Token {
    TokenKind kind;
    uint16_t sourceLen;
    uint32_t index;
};

static_assert(sizeof(Token) == 8);

} // namespace prism

#endif // PRISM_SOURCE_TOKEN_H
