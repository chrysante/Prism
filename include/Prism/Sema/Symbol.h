#ifndef PRISM_SEMA_SYMBOL_H
#define PRISM_SEMA_SYMBOL_H

#include <iosfwd>
#include <span>
#include <string>

#include <utl/vector.hpp>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/Scope.h>
#include <Prism/Sema/SymbolFwd.h>

#define OVERRIDE_TYPE(T)                                                       \
    T const* type() const { return cast<T const*>(Value::type()); }

namespace prism {

class Facet;
class SourceContext;
class SemaContext;

class Symbol {
public:
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

void print(Symbol const& symbol, std::ostream& ostream);

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

    SourceContext const& sourceContext() const { return sourceCtx; }

    using AssocScope::associatedScope;

private:
    SourceContext const& sourceCtx;
};

class Type: public Symbol {
protected:
    using Symbol::Symbol;
};

class ValueType: public Type {
protected:
    using Type::Type;
};

class CompositeType: public ValueType, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

protected:
    CompositeType(SymbolType symType, SemaContext& ctx, std::string name,
                  Facet const* facet, Scope* parent);
};

class StructType: public CompositeType {
public:
    explicit StructType(SemaContext& ctx, std::string name, Facet const* facet,
                        Scope* parent):
        CompositeType(SymbolType::StructType, ctx, std::move(name), facet,
                      parent) {}
};

class GenStructTypeInst: public CompositeType {};

class FunctionType: public ValueType {
public:
    explicit FunctionType(Facet const* facet, Scope* parent,
                          Type const* retType,
                          utl::small_vector<Type const*> params):
        ValueType(SymbolType::FunctionType, {}, facet, parent),
        _retType(retType),
        _params(std::move(params)) {}

    Type const* retType() const { return _retType; }

    std::span<Type const* const> params() const { return _params; }

private:
    Type const* _retType;
    utl::small_vector<Type const*> _params;
};

class PointerType: public ValueType {};

class RawPointerType: public PointerType {};

class ReferenceType: public Type {};

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
                       Trait* trait, CompositeType* conforming);

    Trait* trait() { return _trait; }

    /// \overload
    Trait const* trait() const { return _trait; }

    CompositeType* conformingType() { return conf; }

    /// \overload
    CompositeType const* conformingType() const { return conf; }

private:
    Trait* _trait;
    CompositeType* conf;
};

class GenericSymbol: public Symbol {};

class Value: public Symbol {
public:
    ValueType const* type() const { return _type; }

    ValueCat cat() const { return _valueCat; }

protected:
    Value(SymbolType symType, std::string name, Facet const* facet,
          Scope* parent, ValueType const* type, ValueCat valueCat):
        Symbol(symType, std::move(name), facet, parent),
        _type(type),
        _valueCat(valueCat) {}

private:
    ValueType const* _type;
    ValueCat _valueCat;
};

struct FunctionParameter {
    Facet const* facet;
    std::string name;
    Symbol* typeOrConstraint;
};

class Function: public Symbol {
public:
    explicit Function(std::string name, Facet const* facet, Scope* parent,
                      utl::small_vector<FunctionParameter>&& params,
                      Type const* retType);

    std::span<FunctionParameter const> params() const { return _params; }

    Type const* retType() const { return _retType; }

private:
    utl::small_vector<FunctionParameter> _params;
    Type const* _retType;
};

class FunctionImpl: public Function, public detail::AssocScope {
public:
    using AssocScope::associatedScope;

    explicit FunctionImpl(SemaContext& ctx, std::string name,
                          Facet const* facet, Scope* parent,
                          utl::small_vector<FunctionParameter>&& params,
                          Type const* retType);
};

class FuncArg: public Value {
public:
    explicit FuncArg(std::string name, Facet const* facet, Scope* parent,
                     ValueType const* type, ValueCat valueCat):
        Value(SymbolType::FuncArg, std::move(name), facet, parent, type,
              valueCat) {}
};

class GenericValueArg: public Value {};

class GenericTypeArg: public ValueType {};

class Variable: public Value {
public:
    explicit Variable(std::string name, Facet const* facet, Scope* parent,
                      ValueType const* type):
        Value(SymbolType::Variable, std::move(name), facet, parent, type,
              LValue) {}
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
                Scope* parent, ValueType const* type, ValueCat valueCat,
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
        PRISM_ASSERT(LHS->type() == RHS->type(),
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

#endif // PRISM_SEMA_SYMBOL_H
