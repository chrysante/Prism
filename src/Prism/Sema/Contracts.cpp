#include "Prism/Sema/Contracts.h"

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

TypeObligation::TypeObligation(Trait* trait, Symbol* owner):
    Obligation(SpecType::TypeObligation, trait, owner) {}

FuncObligation::FuncObligation(Function* func, Symbol* owner):
    Obligation(SpecType::FuncObligation, func, owner) {}

Function* FuncObligation::function() const { return cast<Function*>(symbol()); }

InterfaceLike::InterfaceLike() = default;

InterfaceLike::~InterfaceLike() = default;

void InterfaceLike::addObligation(csp::unique_ptr<Obligation> obl) {
    if (!obl) return;
    bag.push_back(std::move(obl));
    visit(*bag.back(), FN1(this, addObligationImpl(&_1)));
}

void InterfaceLike::addObligationImpl(FuncObligation* obl) {
    auto* F = obl->function();
    auto& list = obls[{ F->name(), F->signature() }];
    list.push_back(obl);
}
