#ifndef PRISM_SEMA2_SYMBOL_H
#define PRISM_SEMA2_SYMBOL_H

#include <string>

#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>
#include <utl/ptr_union.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Ranges.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema2/FuncSig.h>
#include <Prism/Sema2/SemaFwd.h>
#include <Prism/Sema2/TypeLayout.h>

#define FACET_TYPE(Type)                                                       \
    template <typename T = Type>                                               \
    T const* facet() const {                                                   \
        return cast<T const*>(Symbol::facet());                                \
    }

namespace prism {

class SourceContext;

///
class ScopeArg {
public:
    static ScopeArg const None;

    static ScopeArg share(Scope* scope) {
        return ScopeArg(PtrUnion(std::in_place_index<1>, scope));
    }

    static ScopeArg make(SemaContext& ctx) { return ScopeArg(&ctx); }

    explicit ScopeArg(Scope* scope): value(std::in_place_index<0>, scope) {}

    Scope* eval(Symbol* defining_symbol) const;

private:
    enum class NoneVal : int;
    using PtrUnion = utl::ptr_union</* owning */ Scope*, /* sharing */ Scope*,
                                    SemaContext*, NoneVal const*>;

    constexpr ScopeArg(PtrUnion value): value(value) {}

    PtrUnion value;
};

inline ScopeArg const ScopeArg::None{ (NoneVal*)nullptr };

/// Base class of all semantic constructs in the compiler
class Symbol {
public:
    std::string const& name() const { return _name; }

    /// \Return the corresponding parse tree node
    Facet const* facet() const { return _facet; }

    /// \Returns the parent scope
    Scope* parent_scope() { return _parent; }

    /// \overload
    Scope const* parent_scope() const { return _parent; }

    /// \Returns the scope of this symbol. May be null
    Scope* scope() { return _assoc_scope; }

    /// \overload
    Scope const* scope() const { return _assoc_scope; }

    /// True if this symbol cannot be found by name lookup
    bool excluded_from_name_lookup() const {
        return get_flag(ExcludeFromNameLookup);
    }

protected:
    enum Flag : uint32_t { ExcludeFromNameLookup };

    Symbol(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
           std::string name, ScopeArg scope_arg);

    void set_name(std::string name) { _name = std::move(name); }

    void set_flag(Flag flag, bool value) { _flags[(size_t)flag] = value; }

private:
    friend struct FuncAnaCtx;
    friend SymbolType get_rtti(Symbol const& This) { return This._sym_type; }

    bool get_flag(Flag flag) const { return _flags[(size_t)flag]; }

    SymbolType _sym_type;
    std::bitset<8> _flags{};
    std::string _name;
    Facet const* _facet;
    Scope* _parent;
    Scope* _assoc_scope;
};

/// Translation unit
class Module final: public Symbol {
public:
    explicit Module(SemaContext& ctx):
        Symbol(SymbolType::Module, nullptr, nullptr, "__MODULE__",
               ScopeArg::make(ctx)) {}
};

/// Multiple source files can make up a compilation unit
class SourceFile: public Symbol {
public:
    explicit SourceFile(Facet const* facet, Scope* parent_scope,
                        ScopeArg scope_arg,
                        SourceContext const& source_context);

    FACET_TYPE(SourceFileFacet)

    SourceContext const& source_context() const { return _source_context; }

private:
    SourceContext const& _source_context;
};

// MARK: Decls

/// Base class of all declarations
class DeclSymbol: public Symbol {
public:
    /// The generic parameter of this declaration. For non-generic declarations
    /// this is empty.
    std::span<GenericParam const> generic_params() const {
        return _generic_params;
    }

    /// True if this declaration has generic parameters
    bool is_generic() const { return !generic_params().empty(); }

    /// The canonical instantiation of this definition, i.e., the defined
    /// type/trait/function. This is only non-null if this declaration is not
    /// generic.
    Symbol* canonical() const { return _canonical; }

protected:
    DeclSymbol(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
               std::string name, ScopeArg scope_arg, size_t num_generic_params):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg),
        _generic_params(num_generic_params) {}

    void set_canonical(Symbol* canonical) { _canonical = canonical; }

private:
    friend struct NameResolution;

    Symbol* _canonical = nullptr;
    utl::small_vector<GenericParam, 3> _generic_params;
};

/// User definition of a struct type
class StructDef final: public DeclSymbol {
public:
    /// Constructor for generic definitions
    explicit StructDef(SemaContext& ctx, Facet const* facet,
                       Scope* parent_scope, std::string name,
                       ScopeArg scope_arg, size_t num_generic_params);

    FACET_TYPE(CompTypeDeclFacet)

    /// The canonical instantiation of this struct, i.e., the defined type. This
    /// is only non-null if this declaration is not generic.
    template <typename SI = StructInst>
    SI* canonical() const {
        return cast<SI*>(DeclSymbol::canonical());
    }
};

/// User definition of a trait
class TraitDef final: public DeclSymbol {
public:
    /// Constructor for generic definitions
    explicit TraitDef(SemaContext& ctx, Facet const* facet, Scope* parent_scope,
                      std::string name, ScopeArg scope_arg,
                      size_t num_generic_params);

    FACET_TYPE(CompTypeDeclFacet)

    /// The canonical instantiation of this trait, i.e., the defined trait. This
    /// is only non-null if this declaration is not generic.
    template <typename TI = TraitInst>
    TI* canonical() const {
        return cast<TI*>(DeclSymbol::canonical());
    }
};

/// User definition of a function
class FunctionDef final: public DeclSymbol {
public:
    explicit FunctionDef(SemaContext& ctx, Facet const* facet,
                         Scope* parent_scope, std::string name,
                         ScopeArg scope_arg, size_t num_generic_params,
                         size_t num_arguments);

    FACET_TYPE(FuncDeclBaseFacet)

    /// A view over the arguments of this function
    std::span<FunctionArgument* const> arguments() { return _args; }

    /// \overload
    std::span<FunctionArgument const* const> arguments() const { return _args; }

    ///
    Type const* return_type() const { return _return_type; }

    /// The top-level block instruction computed by this function
    BlockInst* body() { return _body; }

    /// \overload
    BlockInst const* body() const { return _body; }

    /// The canonical instantiation of this function, i.e., the defined
    /// function. This is only non-null if this declaration is not generic.
    template <typename FI = FunctionInst>
    FI* canonical() const {
        return cast<FI*>(DeclSymbol::canonical());
    }

private:
    friend struct NameResolution;
    friend struct FuncAnaCtx;

    Type const* _return_type = nullptr;
    utl::small_vector<FunctionArgument*> _args;

    BlockInst* _body = nullptr;
};

// MARK: Types

/// Base class of all types
class Type: public Symbol {
public:
    /// \Returns the memory layout information of this type
    TypeLayout layout() const { return _layout; }

protected:
    Type(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
         std::string name, ScopeArg scope_arg, TypeLayout layout):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg),
        _layout(layout) {}

private:
    TypeLayout _layout;
};

/// Class of all builtin types like `void`, `bool`, `byte`, `iN`, `uN`, `fN`
class BuiltinType final: public Type {
public:
    explicit BuiltinType(Scope* parent_scope, std::string name,
                         ScopeArg scope_arg, TypeLayout layout):
        Type(SymbolType::BuiltinType, /* facet: */ nullptr, parent_scope,
             std::move(name), scope_arg, layout) {}
};

/// Instantiation of a possibly generic struct type
class StructInst final: public Type {
public:
    template <RangeOf<Symbol*> GenArgs = ranges::empty_view<Symbol*>>
    explicit StructInst(Facet const* facet, StructDef* definition,
                        GenArgs&& generic_args = {}):
        Type(SymbolType::StructInst, facet, definition->parent_scope(), {},
             ScopeArg::None,
             // FIXME: compute correct layout here if possible
             TypeLayout::Incomplete),
        _definition(definition),
        _generic_args(ranges::begin(generic_args), ranges::end(generic_args)) {
        set_flag(ExcludeFromNameLookup, true);
        set_name(make_name());
        verify();
    }

    /// The struct definition
    StructDef* definition() { return _definition; }

    /// \overload
    StructDef const* definition() const { return _definition; }

    /// The generic arguments of this instantiation. Empty for non-generic
    /// structs
    std::span<Symbol* const> generic_args() const { return _generic_args; }

private:
    std::string make_name() const;
    void verify() const;

    StructDef* _definition;
    utl::small_vector<Symbol*, 3> _generic_args;
};

/// Generic type parameter
class GenTypeParam final: public Type {
public:
    explicit GenTypeParam(Facet const* facet, Scope* parent_scope,
                          std::string name, Trait const* trait_bound):
        Type(SymbolType::GenTypeParam, facet, parent_scope, std::move(name),
             ScopeArg::None, TypeLayout::Incomplete),
        _trait_bound(trait_bound) {}

    /// The trait requirements of this parameter
    Trait const* trait_bound() const { return _trait_bound; }

private:
    Trait const* _trait_bound;
};

///
class FunctionType final: public Type {
public:
    explicit FunctionType(Scope* parent_scope, FuncSig signature):
        Type(SymbolType::FunctionType, /* facet: */ nullptr, parent_scope,
             make_name(signature), ScopeArg::None, TypeLayout::Incomplete),
        _sig(std::move(signature)) {
        set_flag(ExcludeFromNameLookup, true);
    }

    ///
    FuncSig const& signature() const { return _sig; }

private:
    static std::string make_name(FuncSig const& signature);

    FuncSig _sig;
};

// MARK: Traits

/// Base class of all traits
class Trait: public Symbol {
protected:
    Trait(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
          std::string name, ScopeArg scope_arg):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg) {}
};

///
class BuiltinTrait final: public Trait {
public:
    explicit BuiltinTrait(Scope* parent_scope, std::string name,
                          ScopeArg scope_arg):
        Trait(SymbolType::BuiltinTrait, /* facet: */ nullptr, parent_scope,
              std::move(name), scope_arg) {}
};

/// Instantiation of a trait definition
class TraitInst final: public Trait {
public:
    template <RangeOf<Symbol*> GenArgs = ranges::empty_view<Symbol*>>
    explicit TraitInst(Facet const* facet, TraitDef* definition,
                       GenArgs&& generic_args = {}):
        Trait(SymbolType::TraitInst, facet, definition->parent_scope(), {},
              ScopeArg::share(definition->scope())),
        _definition(definition),
        _generic_args(ranges::begin(generic_args), ranges::end(generic_args)) {
        set_flag(ExcludeFromNameLookup, true);
        set_name(make_name());
        verify();
    }

    /// The struct definition
    TraitDef* definition() { return _definition; }

    /// \overload
    TraitDef const* definition() const { return _definition; }

    /// The generic arguments of this instantiation. Empty for non-generic
    /// traits
    std::span<Symbol* const> generic_args() const { return _generic_args; }

private:
    std::string make_name() const;
    void verify() const;

    TraitDef* _definition;
    utl::small_vector<Symbol*, 3> _generic_args;
};

// MARK: Values

#define VALUE_TYPE(Type)                                                       \
    template <typename T = Type>                                               \
    T const* type() const {                                                    \
        return cast<T const*>(Value::type());                                  \
    }

/// Base class of all values
class Value: public Symbol {
public:
    ~Value();

    /// The type of this value
    Type const* type() const { return _type; }

    ///
    Mutability mutability() const { return _mut; }

    /// Shorthand for `mutability() == Mutability::Mut`
    bool is_mutable() const { return mutability() == Mutability::Mut; }

    /// Shorthand for `mutability() == Mutability::Const`
    bool is_const() const { return mutability() == Mutability::Const; }

    ///
    ValueCat value_category() const { return _value_cat; }

    /// A view over the users of this value
    auto users() { return _users | ranges::views::keys; }

protected:
    Value(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
          std::string name, ScopeArg scope_arg, Type const* type,
          Mutability mutability, ValueCat value_category):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg),
        _type(type),
        _mut(mutability),
        _value_cat(value_category) {}

private:
    friend class User;

    // To be called by User @{
    void register_user(User* user);
    void unregister_user(User* user);
    // @}

    Type const* _type;
    Mutability _mut;
    ValueCat _value_cat;
    utl::hashmap<User*, unsigned> _users;
};

/// Value that uses other values as operands
class User: public Value {
public:
    ~User();

    /// The operands used by this user
    std::span<Value const* const> operands() const { return _operands; }

    /// \overload
    std::span<Value* const> operands() { return _operands; }

    /// The operand at \p index
    Value* operand_at(size_t index) { return _operands[index]; }

    /// \overload
    Value const* operand_at(size_t index) const { return _operands[index]; }

    /// Sets the operand at \p index to \p operand
    void set_operand(size_t index, Value* operand);

protected:
    User(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
         std::string name, ScopeArg scope_arg, Type const* type,
         Mutability mutability, ValueCat value_category, auto&&... operands):
        Value(sym_type, facet, parent_scope, std::move(name), scope_arg, type,
              mutability, value_category) {
        _operands.reserve(sizeof...(operands));
        (insert_operand(_operands, operands), ...);
        register_operands();
    }

    template <std::derived_from<Value> V>
    V* operand_as(size_t index) {
        return cast<V*>(operand_at(index));
    }

    template <std::derived_from<Value> V>
    V const* operand_as(size_t index) const {
        return cast<V const*>(operand_at(index));
    }

private:
    friend class Value;

    void on_value_destruction(Value* operand);

    // Constructor helpers @{
    void insert_operand(utl::vector<Value*>& operands,
                        std::ranges::range auto&& ops_in) {
        for (auto& op: ops_in)
            insert_operand(operands, op);
    }
    void insert_operand(utl::vector<Value*>& operands, auto* op) {
        operands.push_back(op);
    }
    void register_operands();
    // @}

    utl::small_vector<Value*, 2> _operands;
};

/// Non-type generic parameter
class GenValueParam final: public Value {
public:
    explicit GenValueParam(Facet const* facet, Scope* parent_scope,
                           std::string name, Type const* type):
        Value(SymbolType::GenValueParam, facet, parent_scope, std::move(name),
              ScopeArg::None, type, Mutability::Const, ValueCat::LValue) {}
};

///
class FunctionArgument final: public Value {
public:
    explicit FunctionArgument(Facet const* facet, Scope* parent_scope,
                              std::string name, PassingConvention pc,
                              Type const* type):
        Value(SymbolType::FunctionArgument, facet, parent_scope,
              std::move(name), ScopeArg::None, type,
              pc == PassingConvention::In ? Mutability::Const : Mutability::Mut,
              ValueCat::LValue),
        _pc(pc) {}

    ///
    PassingConvention passing_convention() const { return _pc; }

private:
    PassingConvention _pc;
};

// MARK: Constants

/// Base class of constant values
class Constant: public Value {
protected:
    using Value::Value;
};

///
class IntLiteral: public Constant {
public:
    explicit IntLiteral(Facet const* facet, APInt value, Type const* type);

    APInt const& value() const { return _value; }

private:
    APInt _value;
};

// MARK: Functions

/// Base class of all functions
class Function: public Constant {
public:
    VALUE_TYPE(FunctionType)

    /// The signature of this function
    FuncSig const& signature() const { return type()->signature(); }

    /// View over the arguments
    std::span<FuncArgSpec const> arguments() const {
        return signature().arguments();
    }

    /// The return type
    Type const* return_type() const { return signature().return_type(); }

protected:
    Function(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
             std::string name, FunctionType const* type):
        Constant(sym_type, facet, parent_scope, std::move(name), ScopeArg::None,
                 type, Mutability::Const, ValueCat::LValue) {}
};

/// Instantiation of a `FunctionDef`
class FunctionInst final: public Function {
public:
    template <RangeOf<Symbol*> GenArgs = ranges::empty_view<Symbol*>>
    explicit FunctionInst(Facet const* facet, FunctionDef* definition,
                          FunctionType const* type,
                          GenArgs&& generic_args = {}):
        Function(SymbolType::FunctionInst, facet, definition->parent_scope(),
                 definition->name(), type),
        _definition(definition),
        _generic_args(ranges::begin(generic_args), ranges::end(generic_args)) {
        set_flag(ExcludeFromNameLookup, true);
        set_name(make_name());
        verify();
    }

    /// The function definition
    FunctionDef* definition() { return _definition; }

    /// \overload
    FunctionDef const* definition() const { return _definition; }

    /// The generic arguments of this instantiation. Empty for non-generic
    /// functions
    std::span<Symbol* const> generic_args() const { return _generic_args; }

private:
    std::string make_name() const;
    void verify() const;

    FunctionDef* _definition;
    utl::small_vector<Symbol*, 3> _generic_args;
};

// MARK: Instructions

/// Base class of all instructions
class Instruction: public User {
protected:
    using User::User;
};

/// Computation of a compound expression
class BlockInst final: public Instruction {
public:
    using iterator = std::vector<Instruction*>::iterator;
    using const_iterator = std::vector<Instruction*>::const_iterator;

    BlockInst(Facet const* facet, Scope* parent_scope, std::string name,
              ScopeArg scope_arg, Type const* type,
              std::vector<Instruction*> instructions):
        Instruction(SymbolType::BlockInst, facet, parent_scope, std::move(name),
                    scope_arg, type, Mutability::Const, ValueCat::RValue),
        _instructions(std::move(instructions)) {}

    FACET_TYPE(CompoundFacet)

    /// Container interface @{
    iterator begin() { return _instructions.begin(); }
    const_iterator begin() const { return _instructions.begin(); }
    iterator end() { return _instructions.end(); }
    const_iterator end() const { return _instructions.end(); }
    size_t size() const { return _instructions.size(); }
    bool empty() const { return _instructions.empty(); }
    /// @}

private:
    std::vector<Instruction*> _instructions;
};

/// Instruction to mark the value of a block instruction
class YieldInst final: public Instruction {
public:
    explicit YieldInst(SemaContext& ctx, Facet const* facet,
                       Scope* parent_scope, Value* operand);

    Value* operand() { return operand_at(0); }

    Value const* operand() const { return operand_at(0); }
};

/// Resolved function call
class CallInst final: public Instruction {
public:
    explicit CallInst(Facet const* facet, Scope* parent_scope, std::string name,
                      Function* callee, std::span<Value* const> arguments):
        Instruction(SymbolType::CallInst, facet, parent_scope, std::move(name),
                    ScopeArg::None, callee->return_type(), Mutability::Const,
                    ValueCat::RValue, callee, arguments) {}

    /// The called function
    Function* callee() { return operand_as<Function>(0); }

    /// \overload
    Function const* callee() const { return operand_as<Function>(0); }

    /// View over the call arguments
    std::span<Value* const> arguments() { return operands().subspan(1); }

    /// \overload
    std::span<Value const* const> arguments() const {
        return operands().subspan(1);
    }
};

#undef VALUE_TYPE

} // namespace prism

#undef FACET_TYPE

#endif // PRISM_SEMA2_SYMBOL_H
