#include "Prism/Source/Token.h"

#include <ostream>

using namespace prism;

std::ostream& prism::operator<<(std::ostream& str, Token const& token) {
    return str << "{ kind: " << token.kind << ", index: " << token.index
               << ", length: " << token.sourceLen << " }";
}
