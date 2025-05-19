#ifndef PRISM_SEMA_CONTRACTS_H
#define PRISM_SEMA_CONTRACTS_H

#include <vector>

#include <utl/hashtable.hpp>
#include <utl/tiny_ptr_vector.hpp>

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

    ///
    void setSymbol(Symbol* sym) { _sym = sym; }

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
    utl::tiny_ptr_vector<Symbol*> _conf;
};

class TypeObligation: public Obligation {
public:
    explicit TypeObligation(Typedef* type, Symbol* owner);

    /// \Returns the requiring typedef
    Typedef* type() const;
};

class FuncObligation: public Obligation {
public:
    explicit FuncObligation(Function* func, Symbol* owner);

    /// \Returns the requiring function
    Function* function() const;
};

class InterfaceLike;

struct FuncObligationKey {
    std::string_view name;
    FuncSig const& funcSig;

    struct Equal {
        Equal(InterfaceLike const* interface): interface(interface) {}

        bool operator()(FuncObligationKey const& lhs,
                        FuncObligationKey const& rhs) const;

        InterfaceLike const* interface;
    };

    struct Hash {
        Hash(InterfaceLike const* interface): interface(interface) {}

        size_t operator()(FuncObligationKey const& key) const;

        InterfaceLike const* interface;
    };
};

namespace detail {

struct InterfaceCompareImpl;

}

/// Base class for symbols that define and conform to interfaces
class InterfaceLike {
public:
    InterfaceLike(Symbol* symbol);
    ~InterfaceLike();
    InterfaceLike(InterfaceLike const&) = delete;
    InterfaceLike& operator=(InterfaceLike const&) = delete;

    Symbol& symbol() { return *_symbol; }

    Symbol const& symbol() const { return *_symbol; }

    std::span<TypeObligation* const> matchTypeObligation(
        std::string_view name) {
        auto itr = _typeObls.find(name);
        if (itr != _typeObls.end()) return itr->second;
        return {};
    }

    std::span<FuncObligation* const> matchFuncObligation(std::string_view name,
                                                         FuncSig const& sig) {
        auto itr = _funcObls.find({ name, sig });
        if (itr != _funcObls.end()) return itr->second;
        return {};
    }

    void addObligation(csp::unique_ptr<Obligation> obl, SpecAddMode);

    auto const& typeObligations() const { return _typeObls; }

    auto const& funcObligations() const { return _funcObls; }

    /// \Returns true if all obligations are unambiguously implemented
    bool isComplete() const;

    /// \Returns true if all obligations defined by traits are unambiguously
    /// implemented
    bool isCompleteForTraits() const;

    ///
    void addTypeConformance(Typedef const* impl, TypeObligation const* obl);

private:
    friend struct detail::InterfaceCompareImpl;

    bool addObligationImpl(TypeObligation* obl, SpecAddMode mode);
    bool addObligationImpl(FuncObligation* obl, SpecAddMode mode);

    Symbol* _symbol;
    utl::hashmap<std::string, utl::tiny_ptr_vector<TypeObligation*>> _typeObls;
    utl::hashmap<Typedef const*, utl::tiny_ptr_vector<TypeObligation const*>>
        _typedefOblMap;
    utl::hashmap<Type const*, utl::tiny_ptr_vector<Typedef const*>>
        _typedefDefinitionMap;
    utl::hashmap<FuncObligationKey, utl::tiny_ptr_vector<FuncObligation*>,
                 FuncObligationKey::Hash, FuncObligationKey::Equal>
        _funcObls;
    std::vector<csp::unique_ptr<Obligation>> bag;
};

} // namespace prism

#endif // PRISM_SEMA_CONTRACTS_H
