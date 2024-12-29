#include "Prism/Sema/Contracts.h"

#include "Prism/Sema/Symbol.h"

using namespace prism;

TypeObligation::TypeObligation(Trait* trait):
    Obligation(SpecType::TypeObligation, trait) {}

FunctionObligation::FunctionObligation(Function* func):
    Obligation(SpecType::FunctionObligation, func) {}

TypeConformance::TypeConformance(Trait* trait, TypeObligation* obligation):
    Conformance(SpecType::TypeConformance, trait, obligation) {}

FunctionConformance::FunctionConformance(Function* func,
                                         FunctionObligation* obligation):
    Conformance(SpecType::FunctionConformance, func, obligation) {}
