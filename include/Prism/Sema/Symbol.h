#ifndef PRISM_SEMA_SYMBOL_H
#define PRISM_SEMA_SYMBOL_H

#include <bit>
#include <iosfwd>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include <APMath/APInt.h>
#include <utl/hashtable.hpp>
#include <utl/small_ptr_vector.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Assert.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema/Contracts.h>
#include <Prism/Sema/FuncSig.h>
#include <Prism/Sema/QualType.h>
#include <Prism/Sema/Scope.h>
#include <Prism/Sema/SemaFwd.h>
#include <Prism/Sema/TypeLayout.h>

#define FACET_TYPE(Type)                                                       \
    template <typename T = Type>                                               \
    T const* facet() const {                                                   \
        return cast<T const*>(Symbol::facet());                                \
    }

namespace prism {

class SourceContext;
class SemaContext;
class Obligation;
class Conformance;
using APMath::APInt;

class Symbol {
public:
    Symbol(Symbol const&) = delete;
    Symbol& operator=(Symbol const&) = delete;

    std::string const& name() const { return _name; }

    /// \Return the corresponding source code construct
    Facet const* facet() const { return _facet; }

    /// \Returns the parent scope of this symbol
    Scope* parentScope() const { return _parent; }

    Scope* associatedScope() {
        return const_cast<Scope*>(std::as_const(*this).associatedScope());
    }

    Scope const* associatedScope() const;

protected:
    Symbol(SymbolType type, std::string name, Facet const* facet,
           Scope* parent);

    void setSymbolType(SymbolType type) { _symType = type; }

private:
    friend SymbolType get_rtti(Symbol const& sym) { return sym._symType; }

    SymbolType _symType;
    std::string _name;
    Facet const* _facet;
    Scope* _parent;
};

namespace detail {

class AssocScope {
public:
    Scope* associatedScope() { return _scope; }

    Scope const* associatedScope() const { return _scope; }

protected:
    explicit AssocScope(SemaContext& ctx, Scope* scope, Symbol* This);

private:
    Scope* _scope;
};

} // namespace detail

class Module: public Symbol, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

protected:
    Module(SymbolType type, SemaContext& ctx, std::string name,
           Facet const* facet = nullptr, Scope* parent = nullptr);
};

class Target: public Module {
public:
    explicit Target(SemaContext& ctx, std::string name):
        Module(SymbolType::Target, ctx, std::move(name)) {}
};

class Library: public Module {
public:
    explicit Library(SemaContext& ctx, std::string name, Scope* parent):
        Module(SymbolType::Library, ctx, std::move(name), nullptr, parent) {}
};

class SourceFile: public Symbol, public detail::AssocScope {
public:
    explicit SourceFile(SemaContext& ctx, std::string name, Facet const* facet,
                        Scope* parent, SourceContext const& sourceCtx);

    FACET_TYPE(SourceFileFacet)

    SourceContext const& sourceContext() const { return sourceCtx; }

    using AssocScope::associatedScope;

private:
    SourceContext const& sourceCtx;
};

/// Types, traits and functions have a generic context if they are generic
class GenericContext {
public:
    explicit GenericContext(utl::small_vector<Symbol*> params):
        _params(std::move(params)) {}

    ///
    std::span<Symbol* const> params() { return _params; }

    /// \overload
    std::span<Symbol const* const> params() const { return _params; }

private:
    utl::small_vector<Symbol*> _params;
};

namespace detail {

class GenContextBase {
public:
    /// \Returns the generic context if it exists
    GenericContext const* genericContext() const {
        if (_ctx) return &*_ctx;
        return nullptr;
    }

protected:
    void setGenCtx(std::optional<GenericContext> ctx) { _ctx = std::move(ctx); }

private:
    std::optional<GenericContext> _ctx;
};

} // namespace detail

/// Base class of generic versions of composite types, traits, trait impls and
/// functions
class GenericSymbol: public Symbol, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

    ///
    std::span<Symbol* const> genParams() { return _genParams; }

    /// \overload
    std::span<Symbol const* const> genParams() const { return _genParams; }

protected:
    explicit GenericSymbol(SymbolType symType, SemaContext& ctx,
                           std::string name, Facet const* facet, Scope* parent,
                           Scope* scope,
                           utl::small_vector<Symbol*>&& genParams):
        Symbol(symType, std::move(name), facet, parent),
        AssocScope(ctx, scope, this),
        _genParams(std::move(genParams)) {}

private:
    utl::small_vector<Symbol*> _genParams;
};

class Type: public Symbol {
public:
    /// \Returns the memory layout of this type
    TypeLayout layout() const { return _layout; }

protected:
    Type(SymbolType type, std::string name, Facet const* facet, Scope* parent,
         TypeLayout layout):
        Symbol(type, std::move(name), facet, parent), _layout(layout) {}

    void setLayout(TypeLayout layout) { _layout = layout; }

private:
    TypeLayout _layout;
};

class TraitImplInterface;

/// Base class for symbols that can conform to traits. This should probably be
/// only `ValueType`
class TraitConformer {
public:
    /// \Returns the implementation for \p trait if it exists
    TraitImplInterface* findTraitImpl(Trait const* trait) {
        return const_cast<TraitImplInterface*>(
            std::as_const(*this).findTraitImpl(trait));
    }

    /// \overload
    std::span<GenTraitImpl* const> findTraitImpl(GenTrait const* trait) {
        auto result = std::as_const(*this).findTraitImpls(trait);
        return { const_cast<GenTraitImpl* const*>(result.data()),
                 result.size() };
    }

    /// \overload
    TraitImplInterface const* findTraitImpl(Trait const* trait) const;

    /// \overload
    std::span<GenTraitImpl const* const> findTraitImpls(
        GenTrait const* trait) const;

    ///
    void setTraitImpl(TraitImpl& impl);

    ///
    void setTraitImpl(GenTraitImpl& impl);

private:
    utl::hashmap<Trait const*, TraitImplInterface*> _traitImpls;
    utl::hashmap<GenTrait const*, utl::small_ptr_vector<GenTraitImpl*>>
        _genTraitImpls;
};

/// Base class of types that contain "values" as opposed to references and
/// functions
class ValueType: public Type, public TraitConformer {
protected:
    using Type::Type;
};

///
class GenericTypeParam: public ValueType {
public:
    explicit GenericTypeParam(std::string name, Facet const* facet,
                              Scope* parent, Trait* traitBound):
        ValueType(SymbolType::GenericTypeParam, std::move(name), facet, parent,
                  TypeLayout::Incomplete),
        _traitBound(traitBound) {}

    /// \Returns the trait that this type argument conforms to
    Trait* traitBound() { return _traitBound; }

    /// \overload
    Trait const* traitBound() const { return _traitBound; }

private:
    Trait* _traitBound;
};

/// Base class of all types with a scope
class ScopedType: public ValueType, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

protected:
    ScopedType(SymbolType symType, SemaContext& ctx, std::string name,
               Facet const* facet, Scope* parent, Scope* scope,
               TypeLayout layout);
};

/// Base class of all user defined types
class UserType: public ScopedType {
protected:
    using ScopedType::ScopedType;
};

/// Common interface of `CompositeType` and `GenCompositeType`
class CompTypeInterface: public InterfaceLike {
public:
    explicit CompTypeInterface(Symbol* sym): _sym(*sym) {}

    /// \Returns the owning type or generic type
    Symbol& compositeType() { return _sym; }

    /// \overload
    Symbol const& compositeType() const { return _sym; }

    CompTypeInterface& interface() { return *this; }

    CompTypeInterface const& interface() const { return *this; }

    /// \Returns the list of base traits in the order of declaration
    std::span<BaseTrait* const> baseTraits() { return _baseTraits; }

    /// \overload
    std::span<BaseTrait const* const> baseTraits() const { return _baseTraits; }

    /// \Returns the list of base classes in the order of declaration
    std::span<BaseClass* const> baseClasses() { return _bases; }

    /// \overload
    std::span<BaseClass const* const> baseClasses() const { return _bases; }

    /// \Returns the list of non-static member variables in the order of
    /// declaration
    std::span<MemberVar* const> memberVars() { return _memvars; }

    /// \overload
    std::span<MemberVar const* const> memberVars() const { return _memvars; }

private:
    friend struct GlobalNameResolver;
    friend struct InstantiationContext;
    friend struct GenInstContext;
    friend class GenStructTypeInst;

    Symbol& _sym;
    std::vector<BaseTrait*> _baseTraits;
    std::vector<BaseClass*> _bases;
    std::vector<MemberVar*> _memvars;
};

/// Base class of all types with non-static member variables
class CompositeType: public UserType, public CompTypeInterface {
public:
    FACET_TYPE(CompTypeDeclFacet)

protected:
    CompositeType(SymbolType symType, SemaContext& ctx, std::string name,
                  Facet const* facet, Scope* parent, TypeLayout layout):
        UserType(symType, ctx, std::move(name), facet, parent, nullptr, layout),
        CompTypeInterface(this) {}

    friend struct InstantiationContext;
};

/// User defined product type
class StructType: public CompositeType {
public:
    explicit StructType(SemaContext& ctx, std::string name, Facet const* facet,
                        Scope* parent,
                        TypeLayout layout = TypeLayout::Incomplete):
        CompositeType(SymbolType::StructType, ctx, std::move(name), facet,
                      parent, layout) {}
};

/// Base class of all types with non-static member variables
class GenCompositeType: public GenericSymbol, public CompTypeInterface {
public:
    FACET_TYPE(CompTypeDeclFacet)

protected:
    GenCompositeType(SymbolType symType, SemaContext& ctx, std::string name,
                     Facet const* facet, Scope* parent, Scope* scope,
                     utl::small_vector<Symbol*>&& genParams):
        GenericSymbol(symType, ctx, std::move(name), facet, parent, scope,
                      std::move(genParams)),
        CompTypeInterface(this) {}

    friend struct InstantiationContext;
};

/// User defined product type
class GenStructType: public GenCompositeType {
public:
    using InstantiationType = GenStructTypeInst;

    explicit GenStructType(SemaContext& ctx, std::string name,
                           Facet const* facet, Scope* parent, Scope* scope,
                           utl::small_vector<Symbol*>&& genParams):
        GenCompositeType(SymbolType::GenStructType, ctx, std::move(name), facet,
                         parent, scope, std::move(genParams)) {}
};

///
class GenericInstantiation {
public:
    GenericSymbol* genTemplate() { return _templ; }

    GenericSymbol const* genTemplate() const { return _templ; }

    std::span<Symbol* const> genArguments() { return _genArgs; }

    std::span<Symbol const* const> genArguments() const { return _genArgs; }

protected:
    explicit GenericInstantiation(GenericSymbol* genTemplate,
                                  utl::small_vector<Symbol*>&& arguments):
        _templ(genTemplate), _genArgs(std::move(arguments)) {}

private:
    GenericSymbol* _templ;
    utl::small_vector<Symbol*> _genArgs;
};

/// Instantiation of a generic struct type
class GenStructTypeInst: public CompositeType, public GenericInstantiation {
public:
    explicit GenStructTypeInst(SemaContext& ctx, GenStructType* typeTemplate,
                               utl::small_vector<Symbol*>&& arguments);

    GenStructType* typeTemplate() {
        return cast<GenStructType*>(genTemplate());
    }

    GenStructType const* typeTemplate() const {
        return cast<GenStructType const*>(genTemplate());
    }

private:
    GenStructType* _templ;
    utl::small_vector<Symbol*> _arguments;
};

/// Not really sure about this. Do we even need it? And should it be a value
/// type?
class FunctionType: public ValueType {
public:
    explicit FunctionType(Facet const* facet, Scope* parent,
                          Type const* retType,
                          utl::small_vector<Type const*> params):
        ValueType(SymbolType::FunctionType, {}, facet, parent,
                  TypeLayout::Incomplete),
        _retType(retType),
        _params(std::move(params)) {}

    Type const* retType() const { return _retType; }

    std::span<Type const* const> params() const { return _params; }

private:
    Type const* _retType;
    utl::small_vector<Type const*> _params;
};

///
class ByteType: public ScopedType {
public:
    explicit ByteType(SemaContext& ctx, std::string name, Scope* parent):
        ScopedType(SymbolType::ByteType, ctx, std::move(name), nullptr, parent,
                   nullptr, TypeLayout(1)) {}
};

///
class BoolType: public ScopedType {
public:
    explicit BoolType(SemaContext& ctx, std::string name, Scope* parent):
        ScopedType(SymbolType::BoolType, ctx, std::move(name), nullptr, parent,
                   nullptr, TypeLayout(1)) {}
};

/// Common base class of `IntType` and `FloatType`
class ArithmeticType: public ScopedType {
public:
    /// \Returns the number of bits of this type
    size_t bitwidth() const { return layout().size() * 8; }

protected:
    ArithmeticType(SymbolType symType, SemaContext& ctx, std::string name,
                   Scope* parent, size_t bitwidth):
        ScopedType(symType, ctx, std::move(name), nullptr, parent, nullptr,
                   TypeLayout(bitwidth / 8)) {
        PRISM_ASSERT(bitwidth % 8 == 0);
    }
};

///
enum class Signedness { Signed, Unsigned };

///
class IntType: public ArithmeticType {
public:
    IntType(SemaContext& ctx, std::string name, Scope* parent, size_t bitwidth,
            Signedness sign):
        ArithmeticType(SymbolType::IntType, ctx, std::move(name), parent,
                       bitwidth),
        sign(sign) {}

    ///
    Signedness signedness() const { return sign; }

    ///
    bool isSigned() const { return sign == Signedness::Signed; }

    ///
    bool isUnsigned() const { return !isSigned(); }

private:
    Signedness sign;
};

///
class FloatType: public ArithmeticType {
public:
    FloatType(SemaContext& ctx, std::string name, Scope* parent,
              size_t bitwidth):
        ArithmeticType(SymbolType::FloatType, ctx, std::move(name), parent,
                       bitwidth) {}
};

/// Base class of all pointer types
class PointerType: public ValueType {};

///
class RawPointerType: public PointerType {};

///
class ReferenceType: public Type {
public:
    explicit ReferenceType(QualType referred):
        Type(SymbolType::ReferenceType, /* name: */ {}, /* facet: */ nullptr,
             /* scope: */ nullptr, TypeLayout(8)),
        ref(referred) {}

    /// \Return the referred-to qual type, e.g., `mut i32` for a `&mut i32`
    QualType referred() const { return ref; }

private:
    QualType ref;
};

///
class VoidType: public Type {
public:
    explicit VoidType(std::string name, Scope* parent):
        Type(SymbolType::VoidType, std::move(name), nullptr, parent,
             TypeLayout::Incomplete) {}
};

/// Base class of `BaseClass` and `MemberVar`
class MemberSymbol: public Symbol {
public:
    ValueType* type() { return _type; }

    ValueType const* type() const { return _type; }

protected:
    MemberSymbol(SymbolType symType, std::string name, Facet const* facet,
                 ValueType* type, Scope* parent):
        Symbol(symType, std::move(name), facet, parent), _type(type) {}

private:
    friend struct GlobalNameResolver;

    ValueType* _type;
};

class BaseClass: public MemberSymbol {
public:
    explicit BaseClass(Facet const* facet, Scope* parent, UserType* type):
        MemberSymbol(SymbolType::BaseClass, type->name(), facet, type, parent) {
    }

    CompositeType* type() { return cast<CompositeType*>(MemberSymbol::type()); }

    CompositeType const* type() const {
        return cast<CompositeType const*>(MemberSymbol::type());
    }
};

class MemberVar: public MemberSymbol {
public:
    explicit MemberVar(std::string name, Facet const* facet, Scope* parent,
                       ValueType* type):
        MemberSymbol(SymbolType::MemberVar, std::move(name), facet, type,
                     parent) {}
};

/// Common interface of `Trait` and `GenTrait`
class TraitInterface: public InterfaceLike {
public:
    explicit TraitInterface(Symbol* trait): _sym(*trait) {}

    ///
    Symbol& trait() { return _sym; }

    /// \overload
    Symbol const& trait() const { return _sym; }

    TraitInterface& interface() { return *this; }

    TraitInterface const& interface() const { return *this; }

    /// \Returns the list of base traits in the order of declaration
    std::span<BaseTrait* const> baseTraits() { return _baseTraits; }

    /// \overload
    std::span<BaseTrait const* const> baseTraits() const { return _baseTraits; }

private:
    friend struct GlobalNameResolver;
    friend struct GenInstContext;

    Symbol& _sym;
    std::vector<BaseTrait*> _baseTraits;
};

///
class Trait: public Symbol, public detail::AssocScope, public TraitInterface {
public:
    using AssocScope::associatedScope;

protected:
    explicit Trait(SymbolType symType, SemaContext& ctx, std::string name,
                   Facet const* facet, Scope* parent):
        Symbol(symType, std::move(name), facet, parent),
        AssocScope(ctx, nullptr, this),
        TraitInterface(this) {}
};

///
class TraitDef: public Trait {
public:
    explicit TraitDef(SemaContext& ctx, std::string name, Facet const* facet,
                      Scope* parent):
        Trait(SymbolType ::TraitDef, ctx, std::move(name), facet, parent) {}
};

///
class GenTrait: public GenericSymbol, public TraitInterface {
public:
    using InstantiationType = GenTraitInst;

    using AssocScope::associatedScope;

    explicit GenTrait(SemaContext& ctx, std::string name, Facet const* facet,
                      Scope* parent, Scope* scope,
                      utl::small_vector<Symbol*>&& genParams):
        GenericSymbol(SymbolType::GenTrait, ctx, std::move(name), facet, parent,
                      scope, std::move(genParams)),
        TraitInterface(this) {}
};

///
class GenTraitInst: public Trait, public GenericInstantiation {
public:
    explicit GenTraitInst(SemaContext& ctx, GenTrait* traitTemplate,
                          utl::small_vector<Symbol*>&& arguments);

    GenTrait* genTemplate() {
        return cast<GenTrait*>(GenericInstantiation::genTemplate());
    }

    GenTrait const* genTemplate() const {
        return cast<GenTrait const*>(GenericInstantiation::genTemplate());
    }
};

///
class BaseTrait: public Symbol {
public:
    explicit BaseTrait(Facet const* facet, Scope* parent, Trait* trait):
        Symbol(SymbolType::BaseTrait, trait->name(), facet, parent),
        _trait(trait) {}

    /// \Returns the conformance declaration
    Trait* trait() { return _trait; }

    /// \overload
    Trait const* trait() const { return _trait; }

private:
    Trait* _trait;
};

/// Common interface of `TraitImpl` and `GenTraitImpl`
class TraitImplInterface: public InterfaceLike {
public:
    explicit TraitImplInterface(Symbol* traitImpl, Trait* trait,
                                ValueType* conforming):
        _sym(*traitImpl), _trait(trait), _conf(conforming) {}

    ///
    Symbol& traitImpl() { return _sym; }

    /// \overload
    Symbol const& traitImpl() const { return _sym; }

    TraitImplInterface& interface() { return *this; }

    TraitImplInterface const& interface() const { return *this; }

    /// \Returns the trait that is being implemented
    Trait* trait() { return _trait; }

    /// \overload
    Trait const* trait() const { return _trait; }

    /// \Returns the type for which \p trait is implemented
    ValueType* conformingType() { return _conf; }

    /// \overload
    ValueType const* conformingType() const { return _conf; }

private:
    friend struct GlobalNameResolver;

    Symbol& _sym;
    Trait* _trait = nullptr;
    ValueType* _conf = nullptr;
};

///
class TraitImpl:
    public Symbol,
    public detail::AssocScope,
    public TraitImplInterface {
public:
    using AssocScope::associatedScope;

    FACET_TYPE(TraitImplFacet)

protected:
    explicit TraitImpl(SymbolType symType, SemaContext& ctx, Facet const* facet,
                       Scope* parent, Trait* trait, ValueType* conforming):
        Symbol(symType, /* name: */ {}, facet, parent),
        AssocScope(ctx, nullptr, this),
        TraitImplInterface(this, trait, conforming) {}
};

///
class TraitImplDef: public TraitImpl {
public:
    explicit TraitImplDef(SemaContext& ctx, Facet const* facet, Scope* parent,
                          Trait* trait = nullptr,
                          CompositeType* conforming = nullptr):
        TraitImpl(SymbolType::TraitImplDef, ctx, facet, parent, trait,
                  conforming) {}
};

///
class GenTraitImpl: public GenericSymbol, public TraitImplInterface {
public:
    using InstantiationType = GenTraitImplInst;

    using AssocScope::associatedScope;

    explicit GenTraitImpl(SemaContext& ctx, Facet const* facet, Scope* parent,
                          Scope* scope, utl::small_vector<Symbol*>&& genParams,
                          Trait* trait = nullptr,
                          ValueType* conforming = nullptr):
        GenericSymbol(SymbolType::GenTraitImpl, ctx, /* name: */ {}, facet,
                      parent, scope, std::move(genParams)),
        TraitImplInterface(this, trait, conforming) {}

    FACET_TYPE(TraitImplFacet)
};

///
class GenTraitImplInst: public TraitImpl, public GenericInstantiation {
public:
    explicit GenTraitImplInst(SemaContext& ctx, GenTraitImpl* implTemplate,
                              utl::small_vector<Symbol*>&& arguments,
                              Trait* trait, CompositeType* conforming);

    GenTraitImpl* genTemplate() {
        return cast<GenTraitImpl*>(GenericInstantiation::genTemplate());
    }

    GenTraitImpl const* genTemplate() const {
        return cast<GenTraitImpl const*>(GenericInstantiation::genTemplate());
    }
};

///
class DynType: public ValueType {
public:
    ///
    Symbol* underlyingSymbol() { return _underlying; }

    /// \overload
    Symbol const* underlyingSymbol() const { return _underlying; }

protected:
    explicit DynType(SymbolType symtype, Symbol* underlying):
        ValueType(symtype, /* name: */ {}, /* facet: */ nullptr,
                  /* parentScope: */ nullptr, TypeLayout::Incomplete),
        _underlying(underlying) {}

private:
    Symbol* _underlying;
};

///
class DynStructType: public DynType {
public:
    explicit DynStructType(StructType* type):
        DynType(SymbolType::DynStructType, type) {}

    ///
    StructType* structType() { return cast<StructType*>(underlyingSymbol()); }

    /// \overload
    StructType const* structType() const {
        return cast<StructType const*>(underlyingSymbol());
    }
};

///
class DynTraitType: public DynType {
public:
    explicit DynTraitType(Trait* trait):
        DynType(SymbolType::DynTraitType, trait) {}

    /// \Return the trait that this dynamic dynamic type conforms to
    Trait* trait() { return cast<Trait*>(underlyingSymbol()); }

    /// \overload
    Trait const* trait() const {
        return cast<Trait const*>(underlyingSymbol());
    }
};

class Value: public Symbol {
public:
    QualType type() const { return _type; }

    ValueCat cat() const { return _valueCat; }

protected:
    Value(SymbolType symType, std::string name, Facet const* facet,
          Scope* parent, QualType type, ValueCat valueCat):
        Symbol(symType, std::move(name), facet, parent),
        _type(type),
        _valueCat(valueCat) {}

private:
    friend struct GlobalNameResolver;

    QualType _type;
    ValueCat _valueCat;
};

/// Function parameter declaration. This differs from `FuncArg`, because this is
/// not a value and has no scope. It's type is not a `QualType` but a `Type
/// const*`
class FuncParam: public Symbol {
public:
    struct Options {
        bool hasMut;
        bool isThis;
    };

    explicit FuncParam(std::string name, Facet const* facet, Type const* type,
                       Options options):
        Symbol(SymbolType::FuncParam, std::move(name), facet,
               /* scope: */ nullptr),
        _type(type),
        _hasMut(options.hasMut),
        _isThis(options.isThis) {}

    ///
    Type const* type() const { return _type; }

    /// \Warning This only applies of `type()` is a value type
    bool hasMut() const { return _hasMut; }

    /// \Returns true if this parameter is a `this`-parameter
    bool isThis() const { return _isThis; }

    /// \Returns the constructor options used to construct this object
    Options options() const { return { _hasMut, _isThis }; }

    /// \Returns the corresponding argument value if the parameter belongs to a
    /// function implementation
    FuncArg* argument() { return _arg; }

    /// \overload
    FuncArg const* argument() const { return _arg; }

    ///
    void setArgument(FuncArg* arg) { _arg = arg; }

private:
    Type const* _type = nullptr;
    FuncArg* _arg = nullptr;
    bool _hasMut = false;
    bool _isThis = false;
};

///
class FuncInterface {
public:
    explicit FuncInterface(Symbol* function): _func(*function) {}

    explicit FuncInterface(Symbol* function,
                           utl::small_vector<FuncParam*>&& params,
                           Type const* retType);

    /// \Returns the owning symbol, a `Function` or `GenFunction`
    Symbol& function() { return _func; }

    /// \overload
    Symbol const& function() const { return _func; }

    /// \Returns the interface
    FuncInterface& interface() { return *this; }

    /// \overload
    FuncInterface const& interface() const { return *this; }

    /// \Returns the parameters of this function
    std::span<FuncParam* const> params() { return _params; }

    /// \overload
    std::span<FuncParam const* const> params() const { return _params; }

    /// \Returns the parameter at \p index
    FuncParam* paramAt(size_t index) { return params()[index]; }

    /// \overload
    FuncParam const* paramAt(size_t index) const { return params()[index]; }

    /// \Returns the return type of this function
    Type const* retType() const { return signature().retType(); }

    /// \Returns the signature of this function. Only valid after construction
    /// phase
    FuncSig const& signature() const { return _sig; }

private:
    friend struct GlobalNameResolver;

    Symbol& _func;
    utl::small_vector<FuncParam*> _params;
    FuncSig _sig;
};

/// Function declaration
class Function: public Symbol, public FuncInterface {
public:
    explicit Function(std::string name, Facet const* facet, Scope* parent,
                      utl::small_vector<FuncParam*>&& params,
                      Type const* retType);

    explicit Function(std::string name, Facet const* facet, Scope* parent);

    FACET_TYPE(FuncDeclBaseFacet)
};

/// Function implementation
class FunctionImpl: public Function, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

    explicit FunctionImpl(SemaContext& ctx, std::string name,
                          Facet const* facet, Scope* parent,
                          utl::small_vector<FuncParam*>&& params = {},
                          Type const* retType = nullptr);

    FACET_TYPE(FuncDefFacet)
};

/// Implementation of a generic function
class GenFuncImpl: public GenericSymbol, public FuncInterface {
public:
    explicit GenFuncImpl(SemaContext& ctx, std::string name, Facet const* facet,
                         Scope* parent, Scope* scope,
                         utl::small_vector<Symbol*>&& genParams,
                         utl::small_vector<FuncParam*>&& params = {},
                         Type const* retType = nullptr);

    FACET_TYPE(FuncDeclBaseFacet)
};

class FuncArg: public Value {
public:
    explicit FuncArg(std::string name, Facet const* facet, Scope* parent,
                     QualType type, ValueCat valueCat):
        Value(SymbolType::FuncArg, std::move(name), facet, parent, type,
              valueCat) {}

    FACET_TYPE(ParamDeclFacet)
};

class LiteralValue: public Value {
protected:
    using Value::Value;
};

class IntLiteral: public LiteralValue {
public:
    explicit IntLiteral(Facet const* facet, APInt value, IntType const* type);

    APInt const& value() const { return _value; }

    std::string valueAsString(int base = 10) const;

private:
    APInt _value;
};

class Variable: public Value {
public:
    explicit Variable(std::string name, Facet const* facet, Scope* parent,
                      QualType type):
        Value(SymbolType::Variable, std::move(name), facet, parent, type,
              LValue) {}

    FACET_TYPE(VarDeclFacet)
};

class ValueAlias: public Value {
public:
};

class Instruction: public Value {
public:
    std::span<Value* const> operands() { return _operands; }

    std::span<Value const* const> operands() const { return _operands; }

    Value* operandAt(size_t index) {
        return const_cast<Value*>(std::as_const(*this).operandAt(index));
    }

    Value const* operandAt(size_t index) const {
        PRISM_ASSERT(index < operands().size());
        return _operands[index];
    }

protected:
    Instruction(SymbolType symType, std::string name, Facet const* facet,
                Scope* parent, QualType type, ValueCat valueCat,
                utl::small_vector<Value*, 2> operands):
        Value(symType, std::move(name), facet, parent, type, valueCat),
        _operands(std::move(operands)) {}

private:
    utl::small_vector<Value*, 2> _operands;
};

class RetInst: public Instruction {
public:
    explicit RetInst(Scope* parent, Facet const* facet, Value* retval);

    Value* retval() { return operandAt(0); }
    Value const* retval() const { return operandAt(0); }
};

} // namespace prism

#undef FACET_TYPE

#endif // PRISM_SEMA_SYMBOL_H
