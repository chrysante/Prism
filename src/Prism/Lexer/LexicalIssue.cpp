#include "Prism/Lexer/LexicalIssue.h"

#include <ostream>

using namespace prism;

void LexicalIssue::doFormat(std::ostream& str) const { str << "lexical issue"; }
