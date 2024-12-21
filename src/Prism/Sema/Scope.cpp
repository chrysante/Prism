#include "Prism/Sema/Scope.h"

#include "Prism/Common/Assert.h"

using namespace prism;

void Scope::addSymbol(Symbol* symbol) {
    auto [itr, success] = _symbols.insert(symbol);
    PRISM_ASSERT(success, "Symbol has already been added to this scope");
}
