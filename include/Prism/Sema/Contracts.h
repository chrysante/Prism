#ifndef PRISM_SEMA_CONTRACTS_H
#define PRISM_SEMA_CONTRACTS_H

#include <vector>

#include <utl/hashtable.hpp>
#include <utl/small_ptr_vector.hpp>

#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/Rtti.h>
#include <Prism/Sema/FuncSig.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class Obligation;
class TypeObligation;
class FuncObligation;

enum class SpecType {
    Obligation,
    TypeObligation,
    FuncObligation,
};

PRISM_DEFINE_ENUM_FUNCTIONS(SpecType)

} // namespace prism

PRISM_DEFINE_RTTI(prism::Obligation, prism::SpecType::Obligation, void,
                  Abstract)
PRISM_DEFINE_RTTI(prism::TypeObligation, prism::SpecType::TypeObligation,
                  prism::Obligation, Concrete)
PRISM_DEFINE_RTTI(prism::FuncObligation, prism::SpecType::FuncObligation,
                  prism::Obligation, Concrete)

namespace prism {

enum class SpecAddMode { Inherit, Define };

/// Denotes an definition obligation defined by a trait or base class for a
/// derived class or a conforming type
class Obligation {
public:
    /// \Returns the requiring symbol
    Symbol* symbol() const { return _sym; }

    /// The trait or struct defining this obligation
    Symbol* owner() const { return _owner; }

    Symbol* singleConformance() const {
        return _conf.size() == 1 ? _conf.front() : nullptr;
    }

    std::span<Symbol* const> conformances() const { return _conf; }

    void addConformance(Symbol* sym, SpecAddMode mode);

protected:
    explicit Obligation(SpecType type, Symbol* symbol, Symbol* owner):
        _type(type), _sym(symbol), _owner(owner) {}

private:
    friend SpecType get_rtti(Obligation const& obl) { return obl._type; }

    SpecType _type;
    Symbol* _sym;
    Symbol* _owner;
    utl::small_ptr_vector<Symbol*> _conf;
};

class TypeObligation: public Obligation {
public:
    explicit TypeObligation(Trait* trait, Symbol* owner);
};

class FuncObligation: public Obligation {
public:
    explicit FuncObligation(Function* func, Symbol* owner);

    /// \Returns the requiring function
    Function* function() const;
};

struct FuncObligationKey {
    std::string_view name;
    FuncSig const& funcSig;

    bool operator==(FuncObligationKey const& rhs) const {
        return name == rhs.name && funcSig.compareEqIgnoringFirst(rhs.funcSig);
    }
};

/// Base class for symbols that define and conform to interfaces
class InterfaceLike {
public:
    InterfaceLike();
    ~InterfaceLike();
    InterfaceLike(InterfaceLike const&) = delete;
    InterfaceLike& operator=(InterfaceLike const&) = delete;

    std::span<FuncObligation* const> matchObligation(std::string_view name,
                                                     FuncSig const& sig) {
        auto itr = obls.find({ name, sig });
        if (itr != obls.end()) return itr->second;
        return {};
    }

    void addObligation(csp::unique_ptr<Obligation> obl, SpecAddMode);

    auto const& obligations() const { return obls; }

    /// \Returns true if all obligations are unambiguously implemented
    bool isComplete() const;

private:
    bool addObligationImpl(TypeObligation*, SpecAddMode) { return false; }
    bool addObligationImpl(FuncObligation* obl, SpecAddMode mode);

    utl::hashmap<FuncObligationKey, utl::small_ptr_vector<FuncObligation*>>
        obls;
    std::vector<csp::unique_ptr<Obligation>> bag;
};

} // namespace prism

template <>
struct std::hash<prism::FuncObligationKey> {
    size_t operator()(prism::FuncObligationKey const& key) const {
        size_t seed = key.funcSig.hashValueIgnoringFirst();
        utl::hash_combine(seed, key.name);
        return seed;
    }
};

#endif // PRISM_SEMA_CONTRACTS_H
