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
    if (layout.isIncomplete()) return str << "Incomplete";
    if (layout.isPoison()) return str << "Poison";
    return str << "size: " << layout.size() << ", stride: " << layout.stride()
               << ", align: " << layout.alignment();
}
