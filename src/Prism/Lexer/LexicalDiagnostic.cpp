#include "Prism/Lexer/LexicalDiagnostic.h"

#include <array>
#include <ostream>

using namespace prism;

static Diagnostic::Kind toSeverity(LexicalDiagnostic::Reason reason) {
    return std::array{
#define X(Name, Kind) Diagnostic::Kind,
        LEXICAL_ISSUE_REASON(X)
#undef X
    }[(unsigned)reason];
}

LexicalDiagnostic::LexicalDiagnostic(Reason reason, SourceRange sourceRange,
                                     SourceContext const& sourceContext):
    Diagnostic(toSeverity(reason), sourceRange, &sourceContext),
    _reason(reason) {}

void LexicalDiagnostic::header(std::ostream& str, SourceContext const*) const {
    str << "lexical diag";
}
