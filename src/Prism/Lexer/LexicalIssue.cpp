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

LexicalIssue::LexicalIssue(Reason reason, Token tok):
    Issue(toSeverity(reason), tok.index), _reason(reason), tok(tok) {}

void LexicalIssue::doFormat(std::ostream& str, SourceContext const& ctx) const {
    str << "lexical issue";
}
