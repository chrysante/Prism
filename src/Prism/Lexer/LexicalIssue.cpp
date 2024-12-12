#include "Prism/Lexer/LexicalIssue.h"

#include <ostream>

using namespace prism;

LexicalIssue::LexicalIssue(Reason reason, Token tok):
    Issue(tok.index), _reason(reason), tok(tok) {
    message(MessageKind::Primary,
            [](std::ostream& str) { str << "lexical issue"; });
}
