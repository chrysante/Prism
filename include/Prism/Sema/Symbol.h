#ifndef PRISM_SEMA_SYMBOL_H
#define PRISM_SEMA_SYMBOL_H

#include <iosfwd>
#include <optional>
#include <span>
#include <string>

#include <utl/vector.hpp>

#include <Prism/Common/Assert.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema/QualType.h>
#include <Prism/Sema/Scope.h>
#include <Prism/Sema/SymbolFwd.h>

#define FACET_TYPE(Type)                                                       \
    template <typename T = Type>                                               \
    T const* facet() const {                                                   \
        return cast<T const*>(Symbol::facet());                                \
    }

namespace prism {

class SourceContext;
class SemaContext;

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
    Scope* associatedScope() { return s; }

    Scope const* associatedScope() const { return s; }

protected:
    explicit AssocScope(Scope* s, Symbol* This): s(s) {
        PRISM_ASSERT(s);
        s->_assocSymbol = This;
    }

private:
    Scope* s;
};

} // namespace detail

class Module: public Symbol, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

protected:
    Module(SymbolType type, SemaContext& ctx, std::string name);
};

class Target: public Module {
public:
    explicit Target(SemaContext& ctx, std::string name):
        Module(SymbolType::Target, ctx, std::move(name)) {}
};

class Library: public Module {
public:
    explicit Library(SemaContext& ctx, std::string name):
        Module(SymbolType::Library, ctx, std::move(name)) {}
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

/// Base class of types that contain "values" as opposed to references and
/// functions
class ValueType: public Type {
protected:
    using Type::Type;
};

/// Base class of all types with a scope
class ScopedType: public ValueType, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

protected:
    ScopedType(SymbolType symType, SemaContext& ctx, std::string name,
               Facet const* facet, Scope* parent, TypeLayout layout);
};

/// Base class of all user defined types
class UserType: public ScopedType {
protected:
    using ScopedType::ScopedType;
};

/// Base class of all types with non-static member variables
class CompositeType: public UserType {
public:
    FACET_TYPE(CompTypeDeclFacet)

    /// \Returns the list of base classes in the order of declaration
    std::span<BaseClass* const> bases() { return _bases; }

    /// \overload
    std::span<BaseClass const* const> bases() const { return _bases; }

    /// \Returns the list of non-static member variables in the order of
    /// declaration
    std::span<MemberVar* const> memberVars() { return _memvars; }

    /// \overload
    std::span<MemberVar const* const> memberVars() const { return _memvars; }

protected:
    using UserType::UserType;

private:
    friend struct GlobalNameResolver;
    friend struct InstantiationContext;

    std::vector<BaseClass*> _bases;
    std::vector<MemberVar*> _memvars;
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

/// Instantiation of a struct type
class GenStructTypeInst: public CompositeType {};

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
                   TypeLayout(1)) {}
};

///
class BoolType: public ScopedType {
public:
    explicit BoolType(SemaContext& ctx, std::string name, Scope* parent):
        ScopedType(SymbolType::BoolType, ctx, std::move(name), nullptr, parent,
                   TypeLayout(1)) {}
};

/// Common base class of `IntType` and `FloatType`
class ArithmeticType: public ScopedType {
public:
    /// \Returns the number of bits of this type
    size_t bitwidth() const { return layout().size() * 8; }

protected:
    ArithmeticType(SymbolType symType, SemaContext& ctx, std::string name,
                   Scope* parent, size_t bitwidth):
        ScopedType(symType, ctx, std::move(name), nullptr, parent,
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
                       bitwidth) {}

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
    ValueType const* type() const { return _type; }

protected:
    MemberSymbol(SymbolType symType, std::string name, Facet const* facet,
                 ValueType const* type, Scope* parent):
        Symbol(symType, std::move(name), facet, parent), _type(type) {}

private:
    friend struct GlobalNameResolver;

    ValueType const* _type;
};

class BaseClass: public MemberSymbol {
public:
    explicit BaseClass(Facet const* facet, Scope* parent, UserType const* type):
        MemberSymbol(SymbolType::BaseClass, type->name(), facet, type, parent) {
    }

    UserType const* type() const {
        return cast<UserType const*>(MemberSymbol::type());
    }
};

class MemberVar: public MemberSymbol {
public:
    explicit MemberVar(std::string name, Facet const* facet, Scope* parent,
                       ValueType const* type):
        MemberSymbol(SymbolType::MemberVar, std::move(name), facet, type,
                     parent) {}
};

class Trait: public Symbol, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

    explicit Trait(SemaContext& ctx, std::string name, Facet const* facet,
                   Scope* parent);
};

class TraitImpl: public Symbol, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

    explicit TraitImpl(SemaContext& ctx, Facet const* facet, Scope* parent,
                       Trait* trait, UserType* conforming);

    FACET_TYPE(TraitImplFacet)

    Trait* trait() { return _trait; }

    /// \overload
    Trait const* trait() const { return _trait; }

    UserType* conformingType() { return _conf; }

    /// \overload
    UserType const* conformingType() const { return _conf; }

private:
    friend struct GlobalNameResolver;

    Trait* _trait;
    UserType* _conf;
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

class GenericSymbol: public Symbol {};

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
    explicit FuncParam(std::string name, Facet const* facet, Type const* type,
                       bool hasMut):
        Symbol(SymbolType::FuncParam, std::move(name), facet,
               /* scope: */ nullptr),
        _type(type),
        _hasMut(hasMut) {}

    ///
    Type const* type() const { return _type; }

    /// \Warning This only applies of `type()` is a value type
    bool hasMut() const { return _hasMut; }

private:
    Type const* _type;
    bool _hasMut;
};

/// Function declaration
class Function: public Symbol {
public:
    explicit Function(std::string name, Facet const* facet, Scope* parent,
                      utl::small_vector<FuncParam*>&& params,
                      Type const* retType);

    FACET_TYPE(FuncDeclBaseFacet)

    /// \Returns the parameters of this function
    std::span<FuncParam* const> params() { return _params; }

    /// \overload
    std::span<FuncParam const* const> params() const { return _params; }

    /// \Returns the return type of this function
    Type const* retType() const { return _retType; }

private:
    friend struct GlobalNameResolver;

    utl::small_vector<FuncParam*> _params;
    Type const* _retType;
};

/// Function implementation
class FunctionImpl: public Function, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

    explicit FunctionImpl(SemaContext& ctx, std::string name,
                          Facet const* facet, Scope* parent,
                          utl::small_vector<FuncParam*>&& params,
                          Type const* retType);

    FACET_TYPE(FuncDefFacet)
};

class FuncArg: public Value {
public:
    explicit FuncArg(std::string name, Facet const* facet, Scope* parent,
                     QualType type, ValueCat valueCat):
        Value(SymbolType::FuncArg, std::move(name), facet, parent, type,
              valueCat) {}

    FACET_TYPE(ParamDeclFacet)
};

class GenericValueArg: public Value {};

class GenericTypeArg: public ValueType {};

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

class Computation: public Value {
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
    Computation(SymbolType symType, std::string name, Facet const* facet,
                Scope* parent, QualType type, ValueCat valueCat,
                utl::small_vector<Value*, 2> operands):
        Value(symType, std::move(name), facet, parent, type, valueCat),
        _operands(std::move(operands)) {}

private:
    utl::small_vector<Value*, 2> _operands;
};

class ArithmeticComputation: public Computation {
public:
    explicit ArithmeticComputation(std::string name, Facet const* facet,
                                   Scope* parent, ArithmeticOperation operation,
                                   Value* RHS, Value* LHS):
        Computation(SymbolType::ArithmeticComputation, std::move(name), facet,
                    parent, LHS->type(), ValueCat::RValue, { LHS, RHS }),
        op(operation) {
        PRISM_ASSERT(LHS->type().get() == RHS->type().get(),
                     "Operands must have the same type");
    }

    ArithmeticOperation operation() const { return op; }

    Value* RHS() { return operandAt(0); }
    Value const* RHS() const { return operandAt(0); }

    Value* LHS() { return operandAt(1); }
    Value const* LHS() const { return operandAt(1); }

private:
    ArithmeticOperation op;
};

} // namespace prism

#undef FACET_TYPE

#endif // PRISM_SEMA_SYMBOL_H
