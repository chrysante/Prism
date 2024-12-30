#ifndef PRISM_SEMA_CONTRACTS_H
#define PRISM_SEMA_CONTRACTS_H

#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/Rtti.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class Specification;
class Obligation;
class TypeObligation;
class FunctionObligation;
class Conformance;
class FunctionConformance;
class TypeConformance;

enum class SpecType {
    Specification,
    Obligation,
    TypeObligation,
    FunctionObligation,
    Conformance,
    FunctionConformance,
    TypeConformance,
};

PRISM_DEFINE_ENUM_FUNCTIONS(SpecType)

} // namespace prism

PRISM_DEFINE_RTTI(prism::Specification, prism::SpecType::Specification, void,
                  Abstract)
PRISM_DEFINE_RTTI(prism::Obligation, prism::SpecType::Obligation,
                  prism::Specification, Abstract)
PRISM_DEFINE_RTTI(prism::TypeObligation, prism::SpecType::TypeObligation,
                  prism::Obligation, Concrete)
PRISM_DEFINE_RTTI(prism::FunctionObligation,
                  prism::SpecType::FunctionObligation, prism::Obligation,
                  Concrete)
PRISM_DEFINE_RTTI(prism::Conformance, prism::SpecType::Conformance,
                  prism::Specification, Abstract)
PRISM_DEFINE_RTTI(prism::FunctionConformance,
                  prism::SpecType::FunctionConformance, prism::Conformance,
                  Concrete)
PRISM_DEFINE_RTTI(prism::TypeConformance, prism::SpecType::TypeConformance,
                  prism::Conformance, Concrete)

namespace prism {

/// Base class of `Obligation` and `Conformance`
class Specification {
public:
    /// \Returns the requiring or conforming symbol
    Symbol* symbol() const { return _sym; }

protected:
    explicit Specification(SpecType type, Symbol* symbol):
        _type(type), _sym(symbol) {}

private:
    friend SpecType get_rtti(Specification const& spec) { return spec._type; }

    SpecType _type;
    Symbol* _sym;
};

/// Denotes an definition obligation defined by a trait or base class for a
/// derived class or a conforming type
class Obligation: public Specification {
    using Specification::Specification;
};

class TypeObligation: public Obligation {
public:
    explicit TypeObligation(Trait* trait);
};

class FunctionObligation: public Obligation {
public:
    explicit FunctionObligation(Function* func);
};

/// Denotes a conformance of a derived class or a conforming type for an
/// obligation defined by a base class or trait
class Conformance: public Specification {
public:
    Obligation* obligation() const { return _obl; }

protected:
    explicit Conformance(SpecType type, Symbol* symbol, Obligation* obligation):
        Specification(type, symbol), _obl(obligation) {}

private:
    Obligation* _obl;
};

class TypeConformance: public Conformance {
public:
    explicit TypeConformance(Trait* trait, TypeObligation* obligation);
};

class FunctionConformance: public Conformance {
public:
    explicit FunctionConformance(Function* func,
                                 FunctionObligation* obligation);
};

} // namespace prism

#endif // PRISM_SEMA_CONTRACTS_H
