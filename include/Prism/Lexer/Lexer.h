#ifndef PRISM_LEXER_LEXER_H
#define PRISM_LEXER_LEXER_H

#include <cstdint>
#include <optional>
#include <string_view>

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Source/SourceContext.h>
#include <Prism/Source/Token.h>

namespace prism {

struct Lexer {

    explicit Lexer(SourceContext const& sourceContext, DiagnosticEmitter& DE):
        sourceContext(sourceContext), DE(DE), source(sourceContext.source()) {}

    Token next();

private:
    std::optional<Token> nextImpl();

    std::optional<Token> lexPunctuation();
    std::optional<Token> lexAutoArg();
    std::optional<Token> lexOperator();
    std::optional<Token> lexStringLiteralImpl(TokenKind kind,
                                              std::string_view begin,
                                              std::string_view end);
    std::optional<Token> lexStringLiteral();
    std::optional<Token> lexCharLiteral();
    std::optional<Token> lexIntLiteralImpl(TokenKind kind,
                                           std::string_view prefix,
                                           auto isValidChar);
    std::optional<Token> lexIntLiteral();
    std::optional<Token> lexKeywordOrID();

    void ignoreWhitespace();
    bool ignoreComment();
    void increment();
    void increment(uint32_t& loc) const;

    bool match(std::string_view text);
    bool valid() const;
    bool valid(uint32_t loc) const;
    char current() const;
    char current(uint32_t loc) const;
    std::string_view tokenSource(uint32_t begin, uint32_t end) const;

    SourceContext const& sourceContext;
    DiagnosticEmitter& DE;
    std::string_view source;
    uint32_t index = 0;
};

} // namespace prism

#endif // PRISM_LEXER_LEXER_H
