#ifndef PRISM_SEMA_SYMBOL_H
#define PRISM_SEMA_SYMBOL_H

#include <span>

#include <utl/vector.hpp>

#include <Prism/Sema/SymbolFwd.h>
#include <Prism/Common/Assert.h>

#define OVERRIDE_TYPE(T) T const* type() const { return cast<T const*>(Value::type()); }

namespace prism {

class Scope;
class Facet;

class Symbol {
public:
    /// \Return the corresponding source code construct
    Facet const* facet() const { return _facet; }
    
    /// \Returns the parent scope of this symbol
    Scope* parentScope() const { return _parentScope; }
    
protected:
    Symbol(SymbolType type, Facet const* facet, Scope* parentScope):
        _symType(type), _facet(facet), _parentScope(parentScope) {}
    
private:
    friend SymbolType get_rtti(Symbol const& sym) { return sym._symType; }
    
    SymbolType _symType;
    Facet const* _facet;
    Scope* _parentScope;
};

class Type: public Symbol {
protected:
    using Symbol::Symbol;
};

class ValueType: public Type {
protected:
    using Type::Type;
};

class CompositeType: public ValueType {
protected:
    using ValueType::ValueType;
};

class FunctionType: public ValueType {
public:
    explicit FunctionType(Facet const* facet, Scope* parentScope, Type const* retType,
                          utl::small_vector<Type const*> params):
        ValueType(SymbolType::FunctionType, facet, parentScope), _retType(retType), _params(std::move(params)) {}

    Type const* retType() const { return _retType; }
    
    std::span<Type const* const> params() const { return _params; }
    
private:
    Type const* _retType;
    utl::small_vector<Type const*> _params;
};

class PointerType: public ValueType {
    
};

class RawPointerType: public PointerType {
    
};

class ReferenceType: public Type {
    
};

class Value: public Symbol {
public:
    ValueType const* type() const { return _type; }
    
    ValueCat cat() const { return _valueCat; }
    
protected:
    Value(SymbolType symType, Facet const* facet, Scope* parentScope, ValueType const* type, ValueCat valueCat):
        Symbol(symType, facet, parentScope), _type(type), _valueCat(valueCat) {}
    
private:
    ValueType const* _type;
    ValueCat _valueCat;
};

class Function: public Value {
public:
    OVERRIDE_TYPE(FunctionType)
};

class Argument: public Value {
public:
    
};

class Variable: public Value {
public:
    
};

class ValueAlias: public Value {
public:
    
};

class Computation: public Value {
public:
    std::span<Value* const> operands() { return _operands; }
    
    std::span<Value const* const> operands() const { return _operands; }
    
    Value* operandAt(size_t index) {
        return const_cast<Value* >(std::as_const(*this).operandAt(index));
    }
    
    Value const* operandAt(size_t index) const { PRISM_ASSERT(index < operands().size()); return _operands[index]; }
    
protected:
    Computation(SymbolType symType, Facet const* facet, Scope* parentScope, ValueType const* type, ValueCat valueCat,
                utl::small_vector<Value*, 2> operands):
    Value(symType, facet, parentScope, type, valueCat),
    _operands(std::move(operands)) {}
    
private:
    utl::small_vector<Value*, 2> _operands;
};

class ArithmeticComputation: public Computation {
public:
    explicit ArithmeticComputation(Facet const* facet,
                                   Scope* parentScope,
                                   ArithmeticOperation operation,
                                   Value* RHS, Value* LHS):
        Computation(SymbolType::ArithmeticComputation, facet,
                    parentScope, LHS->type(), ValueCat::RValue, { LHS, RHS }),
        op(operation) { PRISM_ASSERT(LHS->type() == RHS->type(), "Operands must have the same type"); }
    
    ArithmeticOperation operation() const { return op; }
    
    Value* RHS()  { return operandAt(0); }
    Value const* RHS() const { return operandAt(0); }
    
    Value* LHS()  { return operandAt(1); }
    Value const* LHS() const { return operandAt(1); }
    
private:
    ArithmeticOperation op;
};

}

#endif // PRISM_SEMA_SYMBOL_H
