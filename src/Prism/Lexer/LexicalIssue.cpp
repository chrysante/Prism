#include "Prism/Lexer/LexicalIssue.h"

#include <array>
#include <ostream>

using namespace prism;

static Issue::Kind toSeverity(LexicalIssue::Reason reason) {
    return std::array{
#define X(Name, Kind) Issue::Kind,
        LEXICAL_ISSUE_REASON(X)
#undef X
    }[(unsigned)reason];
}

LexicalIssue::LexicalIssue(Reason reason, SourceRange sourceRange):
    Issue(toSeverity(reason), sourceRange), _reason(reason) {}

void LexicalIssue::header(std::ostream& str, SourceContext const*) const {
    str << "lexical issue";
}
