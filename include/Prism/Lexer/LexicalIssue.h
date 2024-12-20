#ifndef PRISM_LEXER_LEXICALISSUE_H
#define PRISM_LEXER_LEXICALISSUE_H

#include <string_view>

#include <Prism/Common/Issue.h>
#include <Prism/Source/Token.h>

namespace prism {

class LexicalIssue: public Issue {
public:
    enum Reason {
        InvalidCharacterSequence,
        UnterminatedStringLiteral,
        InvalidNumericLiteral
    };

    static std::string_view StaticName() { return "LexicalIssue"; }

    explicit LexicalIssue(Reason reason, Token tok);

    Reason reason() const { return _reason; }

    Token token() const { return tok; }

private:
    Reason _reason;
    Token tok;
};

} // namespace prism

#endif // PRISM_LEXER_LEXICALISSUE_H
