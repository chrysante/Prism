#include "Prism/Sema/AnalysisBase.h"

#include "Prism/Facet/Facet.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

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
