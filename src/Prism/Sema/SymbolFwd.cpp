#include "Prism/Sema/SymbolFwd.h"

#include "Prism/Sema/Symbol.h"

using namespace prism;

bool prism::isBuiltinSymbol(Symbol const& sym) {
    if (isa<VoidType>(sym) || isa<ByteType>(sym) || isa<IntType>(sym) ||
        isa<FloatType>(sym))
        return true;
    return false;
}
