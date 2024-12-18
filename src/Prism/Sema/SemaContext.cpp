#include "Prism/Sema/SemaContext.h"

#include "Prism/Sema/Symbol.h"

using namespace prism;

SemaContext::SemaContext() = default;

SemaContext::~SemaContext() = default;

Symbol* SemaContext::addSymbol(csp::unique_ptr<Symbol> sym) {
    auto* s = sym.get();
    symbolBag.push_back(std::move(sym));
    return s;
}
