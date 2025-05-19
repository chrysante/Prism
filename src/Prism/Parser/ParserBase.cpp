#include "Prism/Parser/ParserBase.h"

using namespace prism;

static auto tokEqFn(TokenKind kind) {
    return [kind](TokenKind k) { return k == kind; };
}

static auto tokEqFn(std::span<TokenKind const> kinds) {
    return [kinds](TokenKind k) { return ranges::contains(kinds, k); };
}

std::optional<Token> ParserBase::match(TokenKind kind) {
    return matchImpl(true, tokEqFn(kind));
}

std::optional<Token> ParserBase::match(VolatileList<TokenKind const> kinds) {
    return matchImpl(true, tokEqFn(kinds));
}

std::optional<Token> ParserBase::peekMatch(TokenKind kind, size_t offset) {
    return matchImpl(false, tokEqFn(kind), offset);
}

std::optional<Token> ParserBase::matchImpl(bool eat, auto verify,
                                           size_t offset) {
    auto tok = peek(offset);
    if (!verify(tok.kind)) return std::nullopt;
    if (eat) this->eat(offset);
    return tok;
}

std::optional<Token> ParserBase::peekMatch(VolatileList<TokenKind const> kinds,
                                           size_t offset) {
    return matchImpl(false, tokEqFn(kinds), offset);
}

Token ParserBase::peek(size_t offset) {
    PRISM_ASSERT(offset >= 1);
    return eatPeekImpl(/* increment: */ 0, offset - 1);
}

Token ParserBase::eat(size_t count) {
    return eatPeekImpl(/* increment: */ count, /* offset: */ count - 1);
}

Token ParserBase::eatPeekImpl(size_t increment, size_t offset) {
    while (tokenIndex + offset >= tokens.size())
        tokens.push_back(lexer.next());
    auto tok = tokens[tokenIndex + offset];
    tokenIndex += increment;
    return tok;
}
