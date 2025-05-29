#ifndef PRISM_SEMA2_SYMBOL_H
#define PRISM_SEMA2_SYMBOL_H

#include <string>

#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>
#include <utl/ptr_union.hpp>
#include <utl/type_traits.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Ranges.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema2/FuncSig.h>
#include <Prism/Sema2/SemaFwd.h>
#include <Prism/Sema2/SubContext.h>
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

    template <typename Derived, typename T = utl::copy_cv_t<
                                    Derived, typename Derived::UnusedBaseBytes>>
    static T& unused_bytes(Derived& This) {
        return *reinterpret_cast<T*>(This._unused_bytes.data());
    }

private:
    friend struct FuncAnaCtx;
    friend SymbolType get_rtti(Symbol const& This) { return This._sym_type; }

    bool get_flag(Flag flag) const { return _flags[(size_t)flag]; }

    std::array<uint8_t, 6> _unused_bytes{};
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

/// Imported code
class Library final: public Symbol {
public:
    explicit Library(SemaContext& ctx, Scope* parent_scope, std::string name):
        Symbol(SymbolType::Library, nullptr, parent_scope, std::move(name),
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
    std::span<Symbol* const> generic_params() const { return _generic_params; }

    /// The number of generic parameters
    size_t num_generic_params() const { return _generic_params.size(); }

    ///
    GenericSignature make_generic_signature() const {
        return GenericSignature(generic_params());
    }

    /// True if this declaration has generic parameters
    bool is_generic() const { return !generic_params().empty(); }

    /// The nesting depth of this declaration. This is valid regardless of
    /// whether this declarations is actually generic. Set by name resolution.
    size_t generic_nesting_depth() const { return _generic_nesting_depth; }

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
    uint32_t _generic_nesting_depth = (uint32_t)-1;
    utl::small_vector<Symbol*, 3> _generic_params;
};

/// User definition of a struct type
class StructDef final: public DeclSymbol {
public:
    explicit StructDef(Facet const* facet, Scope* parent_scope,
                       std::string name, ScopeArg scope_arg,
                       size_t num_generic_params):
        DeclSymbol(SymbolType::StructDef, facet, parent_scope, std::move(name),
                   scope_arg, num_generic_params) {}

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
    explicit TraitDef(Facet const* facet, Scope* parent_scope, std::string name,
                      ScopeArg scope_arg, size_t num_generic_params):
        DeclSymbol(SymbolType::TraitDef, facet, parent_scope, std::move(name),
                   scope_arg, num_generic_params) {}

    FACET_TYPE(CompTypeDeclFacet)

    /// The canonical instantiation of this trait, i.e., the defined trait. This
    /// is only non-null if this declaration is not generic.
    template <typename TI = TraitInst>
    TI* canonical() const {
        return cast<TI*>(DeclSymbol::canonical());
    }
};

/// Definition of a trait implementation
class TraitImplDef final: public DeclSymbol {
public:
    explicit TraitImplDef(Facet const* facet, Scope* parent_scope,
                          ScopeArg scope_arg, size_t num_generic_params):
        DeclSymbol(SymbolType::TraitImplDef, facet, parent_scope,
                   /* name: */ {}, scope_arg, num_generic_params) {}

    FACET_TYPE(TraitImplFacet)

    /// The trait being implemented
    Trait* trait() { return _trait; }

    /// \overload
    Trait const* trait() const { return _trait; }

    /// The type for which the trait is implemented
    Type* type() { return _type; }

    /// \overload
    Type const* type() const { return _type; }

private:
    friend struct NameResolution;

    Trait* _trait = nullptr;
    Type* _type = nullptr;
};

/// User definition of a function
class FunctionDef final: public DeclSymbol {
public:
    explicit FunctionDef(Facet const* facet, Scope* parent_scope,
                         std::string name, ScopeArg scope_arg,
                         size_t num_generic_params, size_t num_arguments):
        DeclSymbol(SymbolType::FunctionDef, facet, parent_scope,
                   std::move(name), scope_arg, num_generic_params),
        _args(num_arguments) {}

    FACET_TYPE(FuncDeclBaseFacet)

    /// A view over the arguments of this function
    std::span<FunctionArgument* const> arguments() { return _args; }

    /// \overload
    std::span<FunctionArgument const* const> arguments() const { return _args; }

    ///
    size_t num_arguments() const { return _args.size(); }

    ///
    Type const* return_type() const { return _return_type; }

    /// Creates a `FuncSig` object from the parameters and return type
    FuncSig make_signature() const;

    /// The top-level block instruction computed by this function
    BlockInst* body() { return _body; }

    /// \overload
    BlockInst const* body() const { return _body; }

    ///
    bool has_this_parameter() const;

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

/// Name binding (`var` or `let`) at global or class scope
class BindingDef final: public DeclSymbol {
public:
    explicit BindingDef(Facet const* facet, Scope* parent_scope,
                        std::string name, Type const* type_spec,
                        Value* initializer);

    FACET_TYPE(VarDeclFacet)

    /// The specified type
    Type const* type_spec() const { return _type_spec; }

    /// The evaluated initializer expression
    Value* initializer() { return _init; }

    /// \overload
    Value const* initializer() const { return _init; }

    /// The bound value
    template <typename V = Value>
    V* canonical() const {
        return cast<V*>(DeclSymbol::canonical());
    }

private:
    friend struct NameResolution;

    Type const* _type_spec;
    Value* _init;
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

/// Base class of `StructInst`, `TraitInst` and `FunctionInst`
class InstantiationBase {
public:
    DeclSymbol* definition() { return _def; }

    DeclSymbol const* definition() const { return _def; }

    SubContext const& sub_context() const { return _sub_context; }

    std::span<Symbol* const> generic_args() const {
        return _sub_context.level(_def->generic_nesting_depth());
    }

protected:
    InstantiationBase(DeclSymbol* def, SubContext const& sub_context):
        _def(def), _sub_context(sub_context) {}

private:
    DeclSymbol* _def;
    SubContext _sub_context;
};

///
template <typename Derived>
class InstantiationBaseMixin: public InstantiationBase {
public:
    template <typename D = Derived, typename T = D::DefinitionType>
    T* definition() {
        return cast<T*>(InstantiationBase::definition());
    }

    template <typename D = Derived, typename T = D::DefinitionType>
    T const* definition() const {
        return cast<T const*>(InstantiationBase::definition());
    }

protected:
    using InstantiationBase::InstantiationBase;
};

/// Instantiation of a possibly generic struct type
class StructInst final: public Type, public InstantiationBaseMixin<StructInst> {
public:
    using DefinitionType = StructDef;

    explicit StructInst(StructDef* definition, SubContext const& sub_context);
};

/// The symbolic type the `this` parameter (`this type`) in a trait definition
class TraitThisType final: public Type {
public:
    explicit TraitThisType(Trait* trait):
        Type(SymbolType::TraitThisType, /* facet: */ nullptr,
             /* parent_scope: */ nullptr, "this-type", ScopeArg::None,
             TypeLayout::Incomplete),
        _trait(trait) {}

    ///
    Trait* trait() { return _trait; }

    /// \overload
    Trait const* trait() const { return _trait; }

private:
    Trait* _trait;
};

/// Common base class of `GenTypeParam` and `GenValueParam`
class GenParamBase {
public:
    /// The index of this parameter in the parameter list
    size_t index() const { return _index; }

    /// The nesting depth of the parameter list
    size_t nesting_depth() const { return _nesting_depth; }

private:
    friend class GenTypeParam;
    friend class GenValueParam;

    GenParamBase(size_t index, size_t nesting_depth):
        _index(index), _nesting_depth(nesting_depth) {}

    size_t _index;
    size_t _nesting_depth;
};

/// Generic type parameter
class GenTypeParam final: public Type, public GenParamBase {
public:
    explicit GenTypeParam(Trait const* trait_bound, size_t index,
                          size_t nesting_depth);

    /// The trait requirements of this parameter
    Trait const* trait_bound() const { return _trait_bound; }

    using GenParamBase::index;
    using GenParamBase::nesting_depth;

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

    /// View over the arguments
    std::span<FuncArgSpec const> arguments() const {
        return signature().arguments();
    }

    /// The return type
    Type const* return_type() const { return signature().return_type(); }

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
class TraitInst final: public Trait, public InstantiationBaseMixin<TraitInst> {
public:
    using DefinitionType = TraitDef;

    explicit TraitInst(TraitDef* definition, SubContext const& sub_context);
};

// MARK: Misc

/// Plumbing symbol to return from analysis functions. Can eventually be removed
/// by the call analysis function performing name lookup.
class OverloadSet final: public Symbol {
public:
    explicit OverloadSet(std::string name, utl::small_vector<Symbol*> symbols):
        Symbol(SymbolType::OverloadSet, /* facet: */ nullptr,
               /* parent_scope: */ nullptr, std::move(name), ScopeArg::None),
        _symbols(std::move(symbols)) {}

    /// The symbols in the overload set (`Function` and `FunctionDef`)
    std::span<Symbol* const> symbols() { return _symbols; }

    /// \overload
    std::span<Symbol const* const> symbols() const { return _symbols; }

private:
    utl::small_vector<Symbol*> _symbols;
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
class GenValueParam final: public Value, public GenParamBase {
public:
    explicit GenValueParam(Type const* type, size_t index,
                           size_t nesting_depth);

    using GenParamBase::index;
    using GenParamBase::nesting_depth;
};

///
class FunctionArgument final: public Value {
public:
    explicit FunctionArgument(Facet const* facet, Scope* parent_scope,
                              std::string name, PassingConvention pc,
                              Type const* type, bool is_this):
        Value(SymbolType::FunctionArgument, facet, parent_scope,
              std::move(name), ScopeArg::None, type,
              pc == PassingConvention::In ? Mutability::Const : Mutability::Mut,
              ValueCat::LValue),
        _pc(pc),
        _is_this(is_this) {}

    ///
    PassingConvention passing_convention() const { return _pc; }

    /// Creates a `FuncArgSpec` object from the passing convention and type
    FuncArgSpec make_spec() const { return { passing_convention(), type() }; }

    /// True if this is the `this` parameter
    bool is_this() const { return _is_this; }

private:
    PassingConvention _pc;
    bool _is_this;
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

    /// The number of arguments
    size_t num_arguments() const { return arguments().size(); }

    /// The return type
    Type const* return_type() const { return signature().return_type(); }

protected:
    Function(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
             std::string name, FunctionType const* type):
        Constant(sym_type, facet, parent_scope, std::move(name), ScopeArg::None,
                 type, Mutability::Const, ValueCat::LValue) {}
};

/// Instantiation of a `FunctionDef`
class FunctionInst final:
    public Function,
    public InstantiationBaseMixin<FunctionInst> {
public:
    using DefinitionType = FunctionDef;

    explicit FunctionInst(FunctionDef* definition,
                          SubContext const& sub_context,
                          FunctionType const* type);
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

/// Local name binding
class BindingInst final: public Instruction {
public:
    explicit BindingInst(Facet const* facet, Scope* parent_scope,
                         std::string name, Type const* type,
                         Mutability mutability, Value* initializer):
        Instruction(SymbolType::BindingInst, facet, parent_scope,
                    std::move(name), ScopeArg::None, type, mutability,
                    ValueCat::LValue, initializer) {}

    /// The evaluated initializer expression
    Value* initializer() { return operand_at(0); }

    /// \overload
    Value const* initializer() const { return operand_at(0); }
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

#undef FACET_TYPE

// MARK: Some inline helper functions

inline GenParamBase const* as_gen_param_base(Symbol const* symbol) {
    if (auto* type = dyncast<GenTypeParam const*>(symbol)) return type;
    if (auto* value = dyncast<GenValueParam const*>(symbol)) return value;
    return nullptr;
}

} // namespace prism

#endif // PRISM_SEMA2_SYMBOL_H
