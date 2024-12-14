#include "Prism/Lexer/Lexer.h"

#include <algorithm>
#include <cctype>

#include <range/v3/algorithm.hpp>
#include <utl/hashtable.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Lexer/LexicalIssue.h"

using namespace prism;

static bool isSpace(char c) { return std::isspace(c); }

Token Lexer::next() {
    ignoreWhitespace();
    std::optional<uint32_t> errBegin;
    while (true) {
        uint32_t begin = index;
        if (auto tok = nextImpl()) {
            if (errBegin) {
                uint16_t errLen = begin - *errBegin;
                iss.push<LexicalIssue>(LexicalIssue::InvalidCharacterSequence,
                                       Token{ TokenKind::Error, errLen,
                                              *errBegin });
            }
            return *tok;
        }
        if (!errBegin) {
            errBegin = index;
        }
        increment();
        ignoreWhitespace();
    }
}

std::optional<Token> Lexer::nextImpl() {
    if (!valid()) {
        return Token{ TokenKind::End, 0, index };
    }
    if (auto tok = lexPunctuation()) {
        return tok;
    }
    if (auto tok = lexOperator()) {
        return tok;
    }
    if (auto tok = lexStringLiteral()) {
        return tok;
    }
    if (auto tok = lexCharLiteral()) {
        return tok;
    }
    if (auto tok = lexIntLiteral()) {
        return tok;
    }
    if (auto tok = lexKeywordOrID()) {
        return tok;
    }
    return std::nullopt;
}

std::optional<Token> Lexer::lexPunctuation() {
    auto loc = index;
    switch (current()) {
#define PUNCTUATION_TOKEN_KIND(Kind, Spelling)                                 \
    static_assert(std::string_view(Spelling).size() == 1);                     \
    case Spelling[0]:                                                          \
        increment();                                                           \
        return Token{ TokenKind::Kind, 1, loc };
#include "Prism/Source/Token.def"
    default:
        return std::nullopt;
    }
}

static constexpr auto operatorLetterArrayImpl(auto cont) {
    char all[] =
#define OPERATOR_TOKEN_KIND(Name, Spelling) Spelling
#include "Prism/Source/Token.def"
        ;
    auto begin = std::begin(all);
    auto end = std::end(all);
    std::sort(begin, end);
    auto mid = std::unique(begin, end);
    return cont(begin, mid);
}

static constexpr size_t operatorLetterArraySize() {
    return operatorLetterArrayImpl(ranges::distance);
}

static constexpr auto makeOperatorLetterArray() {
    return operatorLetterArrayImpl([](auto begin, auto end) {
        std::array<char, operatorLetterArraySize()> result;
        std::copy(begin, end, result.begin());
        return result;
    });
}

static bool isOperatorBegin(char c) {
    static constexpr std::array values = makeOperatorLetterArray();
    return ranges::contains(values, c);
}

static std::optional<TokenKind> operatorKind(std::string_view str) {
#define OPERATOR_TOKEN_KIND(Kind, Spelling)                                    \
    if (str == Spelling) {                                                     \
        return TokenKind::Kind;                                                \
    }
#include "Prism/Source/Token.def"
    return std::nullopt;
}

std::optional<Token> Lexer::lexOperator() {
    if (!isOperatorBegin(current())) {
        return std::nullopt;
    }
    uint32_t loc = index;
    std::optional<TokenKind> kind = [&] {
        increment(loc);
        std::optional<TokenKind> currOpKind =
            operatorKind(tokenSource(index, loc));
        while (true) {
            if (!valid(loc)) {
                return currOpKind;
            }
            std::optional<TokenKind> nextOpKind =
                operatorKind(tokenSource(index, loc + 1));
            if (!nextOpKind) {
                return currOpKind;
            }
            currOpKind = nextOpKind;
            increment(loc);
        }
    }();
    if (!kind) {
        return std::nullopt;
    }
    auto beginIndex = index;
    size_t len = loc - index;
    index = loc;
    return Token{ *kind, (uint16_t)len, beginIndex };
}

std::optional<Token> Lexer::lexStringLiteralImpl(TokenKind kind,
                                                 std::string_view beginDelim,
                                                 std::string_view endDelim) {
    if (!source.substr(index).starts_with(beginDelim)) {
        return std::nullopt;
    }
    uint32_t begin = index;
    for (size_t i = 0; i < beginDelim.size(); ++i) {
        increment();
    }
    increment();
    while (true) {
        if (!valid() || current() == '\n') {
            iss.push<LexicalIssue>(LexicalIssue::UnterminatedStringLiteral,
                                   Token{ TokenKind::Error,
                                          (uint16_t)(index - begin), begin });
        }
        if (source.substr(index).starts_with(endDelim) &&
            current(index - 1) != '\\')
        {
            for (size_t i = 0; i < endDelim.size(); ++i) {
                increment();
            }
            return Token{ kind, (uint16_t)(index - begin), begin };
        }
        increment();
    }
}

std::optional<Token> Lexer::lexStringLiteral() {
    return lexStringLiteralImpl(TokenKind::StringLiteral, "\"", "\"");
}

std::optional<Token> Lexer::lexCharLiteral() {
    return lexStringLiteralImpl(TokenKind::CharLiteral, "\'", "\'");
}

std::optional<Token> Lexer::lexIntLiteralImpl(TokenKind kind,
                                              std::string_view prefix,
                                              auto isValidChar) {
    uint32_t begin = index;
    if (!prefix.empty()) {
        if (!source.substr(index).starts_with(prefix)) {
            return std::nullopt;
        }
        index += prefix.size();
        if (!valid() || !std::invoke(isValidChar, current())) {
            iss.push(std::make_unique<LexicalIssue>(
                LexicalIssue::InvalidNumericLiteral,
                Token(TokenKind::Error, begin, (uint32_t)prefix.size())));
            return std::nullopt;
        }
    }
    else {
        if (!valid() || !std::invoke(isValidChar, current())) {
            return std::nullopt;
        }
    }
    while (valid() && std::invoke(isValidChar, current())) {
        increment();
    }
    return Token(kind, index - begin, begin);
}

std::optional<Token> Lexer::lexIntLiteral() {
    static constexpr auto charRange = [](char min, char max) {
        return [=](char c) { return c >= min && c <= max; };
    };
    if (auto tok = lexIntLiteralImpl(TokenKind::IntLiteralBin, "0b",
                                     charRange('0', '1')))
        return tok;
    if (auto tok =
            lexIntLiteralImpl(TokenKind::IntLiteralHex, "0x", [](char c) {
        return charRange('0', '9')(c) || charRange('a', 'f')(c) ||
               charRange('A', 'F')(c);
    }))
        return tok;
    if (auto tok = lexIntLiteralImpl(TokenKind::IntLiteralDec, "",
                                     charRange('0', '9')))
        return tok;
    return std::nullopt;
}

static bool isIDBegin(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isIDContinue(char c) {
    return isIDBegin(c) || (c >= '0' && c <= '9');
}

static utl::hashmap<std::string_view, TokenKind> const KeywordMap = {
#define KEYWORD_TOKEN_KIND(Kind, Spelling) { Spelling, TokenKind::Kind },
#include "Prism/Source/Token.def"
};

std::optional<Token> Lexer::lexKeywordOrID() {
    if (!isIDBegin(current())) {
        return std::nullopt;
    }
    uint32_t loc = index;
    increment();
    while (valid() && isIDContinue(current())) {
        increment();
    }
    size_t len = index - loc;
    std::string_view text = tokenSource(loc, index);
    auto itr = KeywordMap.find(text);
    auto kind = itr != KeywordMap.end() ? itr->second : TokenKind::Identifier;
    return Token{ kind, (uint16_t)len, loc };
}

void Lexer::ignoreWhitespace() {
    while (valid()) {
        if (isSpace(current())) {
            increment();
            continue;
        }
        if (ignoreComment()) continue;
        break;
    }
}

bool Lexer::ignoreComment() {
    if (source.substr(index).starts_with("//")) {
        index += 2;
        while (valid() && current() != '\n')
            increment();
        return true;
    }
    if (source.substr(index).starts_with("/*")) {
        index += 2;
        int level = 1;
        while (valid()) {
            if (source.substr(index).starts_with("/*")) {
                index += 2;
                ++level;
                continue;
            }
            if (source.substr(index).starts_with("*/")) {
                index += 2;
                --level;
                if (level == 0) break;
                continue;
            }
            increment();
        }
        return true;
    }
    return false;
}

void Lexer::increment() { increment(index); }

void Lexer::increment(uint32_t& loc) const { ++loc; }

bool Lexer::valid() const { return valid(index); }

bool Lexer::valid(uint32_t loc) const { return loc < source.size(); }

char Lexer::current() const { return current(index); }

char Lexer::current(uint32_t idx) const {
    PRISM_ASSERT(valid(idx), "invalid index argument");
    return source[idx];
}

std::string_view Lexer::tokenSource(uint32_t begin, uint32_t end) const {
    return source.substr(begin, end - begin);
}
