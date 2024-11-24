#ifndef PRISM_LEXER_LEXICALISSUE_H
#define PRISM_LEXER_LEXICALISSUE_H

#include <Prism/Common/Issue.h>

#include <Prism/Source/Token.h>

namespace prism {

class LexicalIssue: public Issue {
public:
    enum Reason { InvalidCharacterSequence, UnterminatedStringLiteral };

    explicit LexicalIssue(Reason reason, Token tok):
        Issue(tok.index), _reason(reason), tok(tok) {}

    Reason reason() const { return _reason; }

    Token token() const { return tok; }

private:
    void doFormat(std::ostream& str) const override;

    Reason _reason;
    Token tok;
};

} // namespace prism

#endif // PRISM_LEXER_LEXICALISSUE_H
