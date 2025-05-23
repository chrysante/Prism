#ifndef PRISM_SEMA2_SYMBOL_H
#define PRISM_SEMA2_SYMBOL_H

#include <string>

#include <range/v3/view.hpp>
#include <utl/ptr_union.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Ranges.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Sema2/FuncSig.h>
#include <Prism/Sema2/SemaFwd.h>
#include <Prism/Sema2/SymRef.h>
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

    /// The canonical instantiation of this function, i.e., the defined function. This
    /// is only non-null if this declaration is not generic.
    template <typename FI = FunctionInst>
    FI* canonical() const { return cast<FI*>(DeclSymbol::canonical()); }

private:
    utl::small_vector<FunctionArgument*> _args;
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
    /// The type of this value
    Type const* type() const { return _type; }

    ///
    Mutability mutability() const { return _mut; }

    ///
    ValueCat value_category() const { return _value_cat; }

protected:
    Value(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
          std::string name, ScopeArg scope_arg, Type const* type,
          Mutability mutability, ValueCat value_category):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg),
        _type(type),
        _mut(mutability),
        _value_cat(value_category) {}

private:
    Type const* _type;
    Mutability _mut;
    ValueCat _value_cat;
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

// MARK: Functions

/// Base class of all functions
class Function: public Value {
public:
    VALUE_TYPE(FunctionType)

    FuncSig const& signature() const { return type()->signature(); }

protected:
    Function(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
             std::string name, FunctionType const* type):
        Value(sym_type, facet, parent_scope, std::move(name), ScopeArg::None,
              type, Mutability::Const, ValueCat::LValue) {}
};

///
class FunctionInst final: public Function {
public:
    template <RangeOf<Symbol*> GenArgs = ranges::empty_view<Symbol*>>
    explicit FunctionInst(Facet const* facet, FunctionDef* definition,
                          GenArgs&& generic_args = {}):
        Function(SymbolType::FunctionInst, facet, definition->parent_scope(),
                 definition->name(), nullptr),
        _definition(definition),
        _generic_args(ranges::begin(generic_args), ranges::end(generic_args)) {
        set_flag(ExcludeFromNameLookup, true);
#if 0
        set_name(make_name());
        verify();
#endif
    }

    /// The function definition
    FunctionDef* definition() { return _definition; }

    /// \overload
    FunctionDef const* definition() const { return _definition; }

    /// The generic arguments of this instantiation. Empty for non-generic
    /// functions
    std::span<Symbol* const> generic_args() const { return _generic_args; }

private:
    FunctionDef* _definition;
    utl::small_vector<Symbol*, 3> _generic_args;
};

#undef VALUE_TYPE

} // namespace prism

#undef FACET_TYPE

#endif // PRISM_SEMA2_SYMBOL_H
