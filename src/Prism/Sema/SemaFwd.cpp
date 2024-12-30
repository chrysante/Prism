#include "Prism/Sema/SemaFwd.h"

#include "Prism/Facet/Facet.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

bool prism::isBuiltinSymbol(Symbol const& sym) {
    if (isa<VoidType>(sym) || isa<ByteType>(sym) || isa<BoolType>(sym) ||
        isa<IntType>(sym) || isa<FloatType>(sym))
        return true;
    if (auto* trait = dyncast<Trait const*>(&sym))
        return trait->name() == "type"; // Ugh, for now...
    return false;
}

SourceContext const* prism::getSourceContext(Symbol const* sym) {
    if (!sym) return nullptr;
    auto* scope = sym->parentScope();
    while (scope) {
        if (auto* sourceFile = dyncast<SourceFile const*>(scope->assocSymbol()))
            return &sourceFile->sourceContext();
        scope = scope->parent();
    }
    return nullptr;
}
