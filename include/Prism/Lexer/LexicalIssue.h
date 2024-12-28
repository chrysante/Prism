#ifndef PRISM_LEXER_LEXICALISSUE_H
#define PRISM_LEXER_LEXICALISSUE_H

#include <string_view>

#include <Prism/Common/Issue.h>
#include <Prism/Source/Token.h>

namespace prism {

#define LEXICAL_ISSUE_REASON(X)                                                \
    X(InvalidCharacterSequence, Error)                                         \
    X(UnterminatedStringLiteral, Error)                                        \
    X(InvalidNumericLiteral, Error)

class LexicalIssue: public Issue {
public:
    enum Reason {
#define X(Name, ...) Name,
        LEXICAL_ISSUE_REASON(X)
#undef X
    };

    explicit LexicalIssue(Reason reason, SourceRange sourceRange);

    Reason reason() const { return _reason; }

private:
    void header(std::ostream& os, SourceContext const* ctx) const override;

    Reason _reason;
};

} // namespace prism

#endif // PRISM_LEXER_LEXICALISSUE_H
