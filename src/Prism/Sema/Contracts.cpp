#include "Prism/Sema/Contracts.h"

#include <range/v3/algorithm.hpp>

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

void Obligation::addConformance(Symbol* sym, SpecAddMode mode) {
    switch (mode) {
    case SpecAddMode::Define:
        _conf = { sym };
        return;
    case SpecAddMode::Inherit:
        if (!ranges::contains(_conf, sym)) _conf.push_back(sym);
        return;
    }
}

TypeObligation::TypeObligation(Trait* trait, Symbol* owner):
    Obligation(SpecType::TypeObligation, trait, owner) {}

FuncObligation::FuncObligation(Function* func, Symbol* owner):
    Obligation(SpecType::FuncObligation, func, owner) {}

Function* FuncObligation::function() const { return cast<Function*>(symbol()); }

InterfaceLike::InterfaceLike() = default;

InterfaceLike::~InterfaceLike() = default;

void InterfaceLike::addObligation(csp::unique_ptr<Obligation> obl,
                                  SpecAddMode mode) {
    if (!obl) return;
    if (visit(*obl, FN1(&, addObligationImpl(&_1, mode))))
        bag.push_back(std::move(obl));
}

bool InterfaceLike::addObligationImpl(FuncObligation* obl, SpecAddMode mode) {
    auto* F = obl->function();
    auto& list = obls[{ F->name(), F->signature() }];
    switch (mode) {
    case SpecAddMode::Define:
        if (!list.empty()) return false;
        list.push_back(obl);
        return true;
    case SpecAddMode::Inherit:
        if (auto itr = ranges::find(list, F, FN1(_1->function()));
            itr != list.end())
        {
            auto* existing = *itr;
            for (auto* conf: obl->conformances())
                existing->addConformance(conf, SpecAddMode::Inherit);
            return false;
        }
        list.push_back(obl);
        return true;
    }
}

static bool isCompleteImpl(auto& obls, auto filter) {
    for (auto& [key, list]: obls)
        for (auto* obl: list)
            if (filter(obl) && !obl->singleConformance()) return false;
    return true;
}

bool InterfaceLike::isComplete() const {
    return isCompleteImpl(obls, FN1(true));
}

bool InterfaceLike::isCompleteForTraits() const {
    return isCompleteImpl(obls, FN1(isa<Trait>(_1->owner())));
}
