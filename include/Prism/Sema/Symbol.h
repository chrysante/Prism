#ifndef PRISM_SEMA_SYMBOL_H
#define PRISM_SEMA_SYMBOL_H

#include <iosfwd>
#include <span>

#include <utl/vector.hpp>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/Scope.h>
#include <Prism/Sema/SymbolFwd.h>

#define OVERRIDE_TYPE(T)                                                       \
    T const* type() const { return cast<T const*>(Value::type()); }

namespace prism {

class Facet;
class SourceContext;

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
           Scope* parent):
        _symType(type),
        _name(std::move(name)),
        _facet(facet),
        _parent(parent) {}

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
    Module(SymbolType type, std::string name, Scope* scope):
        Symbol(type, std::move(name), nullptr, nullptr),
        AssocScope(scope, this) {}
};

class Target: public Module {
public:
    explicit Target(std::string name, Scope* scope):
        Module(SymbolType::Target, std::move(name), scope) {}
};

class Library: public Module {
public:
    explicit Library(std::string name, Scope* scope):
        Module(SymbolType::Library, std::move(name), scope) {}
};

class SourceFile: public Symbol, public detail::AssocScope {
public:
    explicit SourceFile(std::string name, Facet const* facet,
                        SourceContext const& ctx, Target* target, Scope* scope):
        Symbol(SymbolType::SourceFile, std::move(name), facet,
               target->associatedScope()),
        AssocScope(scope, this),
        ctx(ctx) {}

    SourceContext const& sourceContext() const { return ctx; }

    using AssocScope::associatedScope;

private:
    SourceContext const& ctx;
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
    CompositeType(SymbolType symType, std::string name, Facet const* facet,
                  Scope* parent, Scope* scope):
        ValueType(symType, std::move(name), facet, parent),
        AssocScope(scope, this) {}
};

class StructType: public CompositeType {
public:
    explicit StructType(std::string name, Facet const* facet, Scope* parent,
                        Scope* scope):
        CompositeType(SymbolType::StructType, std::move(name), facet, parent,
                      scope) {}
};

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

class Function: public Value {
public:
    Function(std::string name, Facet const* facet, Scope* parent,
             FunctionType const* type):
        Value(SymbolType::Function, std::move(name), facet, parent, type,
              LValue) {}

    OVERRIDE_TYPE(FunctionType)
};

class Argument: public Value {
public:
};

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
