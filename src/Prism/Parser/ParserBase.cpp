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

std::optional<Token> ParserBase::peekMatch(TokenKind kind) {
    return matchImpl(false, tokEqFn(kind));
}

std::optional<Token> ParserBase::matchImpl(bool eat, auto verify) {
    auto tok = peek();
    if (!verify(tok.kind)) return std::nullopt;
    if (eat) this->eat();
    return tok;
}

std::optional<Token> ParserBase::peekMatch(
    VolatileList<TokenKind const> kinds) {
    return matchImpl(false, tokEqFn(kinds));
}

Token ParserBase::peek() { return eatPeekImpl(0); }

Token ParserBase::eat() { return eatPeekImpl(1); }

Token ParserBase::eatPeekImpl(uint32_t offset) {
    if (tokenIndex < tokens.size()) {
        auto tok = tokens[tokenIndex];
        tokenIndex += offset;
        return tok;
    }
    auto tok = lexer.next();
    tokens.push_back(tok);
    tokenIndex += offset;
    return tok;
}
