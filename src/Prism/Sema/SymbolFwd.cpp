#include "Prism/Sema/SymbolFwd.h"

#include <ostream>

#include "Prism/Sema/Symbol.h"

using namespace prism;

bool prism::isBuiltinSymbol(Symbol const& sym) {
    if (isa<VoidType>(sym) || isa<ByteType>(sym) || isa<IntType>(sym) ||
        isa<FloatType>(sym))
        return true;
    return false;
}

std::ostream& prism::operator<<(std::ostream& str, TypeLayout layout) {
    if (!layout) return str << "Incomplete";
    return str << "size: " << layout.size()
               << ", align: " << layout.alignment();
}
