#ifndef PRISM_SEMA2_SYMBOL_H
#define PRISM_SEMA2_SYMBOL_H

#include <array>
#include <string>

#include <range/v3/view.hpp>
#include <utl/ptr_union.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Ranges.h>
#include <Prism/Common/SyntaxMacros.h>
#include <Prism/Facet/FacetFwd.h>
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
           std::string name, ScopeArg scope_arg):
        _sym_type(sym_type),
        _name(std::move(name)),
        _facet(facet),
        _parent(parent_scope),
        _assoc_scope(scope_arg.eval(this)) {}

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
    explicit Module(ScopeArg scope_arg):
        Symbol(SymbolType::Module, nullptr, nullptr, "MODULE", scope_arg) {}
};

/// Multiple source files can make up a compilation unit
class SourceFile: public Symbol {
public:
    explicit SourceFile(Facet const* facet, Scope* parent_scope,
                        std::string name, ScopeArg scope_arg,
                        SourceContext const& source_context):
        Symbol(SymbolType::SourceFile, facet, parent_scope, std::move(name),
               scope_arg),
        _source_context(source_context) {}

    FACET_TYPE(SourceFileFacet)

    SourceContext const& source_context() const { return _source_context; }

private:
    SourceContext const& _source_context;
};

// MARK: Decls

/// Base class of all declarations
class SymbolDecl: public Symbol {
public:
    /// The generic parameter of this declaration. For non-generic declarations
    /// this is empty.
    std::span<GenericParam const> generic_params() const {
        return _generic_params;
    }

    /// True if this declaration has generic parameters
    bool is_generic() const { return !generic_params().empty(); }

protected:
    SymbolDecl(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
               std::string name, ScopeArg scope_arg,
               RangeOf<GenericParam> auto&& gen_params):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg),
        _generic_params(ranges::begin(gen_params), ranges::end(gen_params)) {}

private:
    utl::small_vector<GenericParam, 3> _generic_params;
};

/// User definition of a struct type
class StructDef final: public SymbolDecl {
public:
    /// Constructor for generic definitions
    explicit StructDef(Facet const* facet, Scope* parent_scope,
                       std::string name, ScopeArg scope_arg,
                       RangeOf<GenericParam> auto&& gen_params):
        SymbolDecl(SymbolType::StructDef, facet, parent_scope, std::move(name),
                   scope_arg, PRISM_FWD(gen_params)) {}

    /// Constructor for non-generic definitions
    explicit StructDef(Facet const* facet, Scope* parent_scope,
                       std::string name, ScopeArg scope_arg);

    FACET_TYPE(CompTypeDeclFacet)

    /// The canonical instantiation of this struct, i.e., the defined type. This
    /// is only non-null if this declaration is not generic.
    StructType* canonical_type() const { return _canonical_type.get(); }

private:
    std::unique_ptr<StructType> _canonical_type;
};

/// User definition of a trait
class TraitDef final: public SymbolDecl {
public:
    /// Constructor for generic definitions
    explicit TraitDef(Facet const* facet, Scope* parent_scope, std::string name,
                      ScopeArg scope_arg,
                      RangeOf<GenericParam> auto&& gen_params):
        SymbolDecl(SymbolType::TraitDef, facet, parent_scope, std::move(name),
                   scope_arg, PRISM_FWD(gen_params)) {}

    /// Constructor for non-generic definitions
    explicit TraitDef(Facet const* facet, Scope* parent_scope, std::string name,
                      ScopeArg scope_arg);

    FACET_TYPE(CompTypeDeclFacet)

    /// The canonical instantiation of this struct, i.e., the defined type. This
    /// is only non-null if this declaration is not generic.
    TraitInst* canonical_trait() const { return _canonical_trait.get(); }

private:
    std::unique_ptr<TraitInst> _canonical_trait;
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
class StructType final: public Type {
public:
    template <RangeOf<SymRef<>> GenArgs = std::array<SymRef<>, 0>>
    explicit StructType(Facet const* facet, StructDef* definition,
                        GenArgs&& generic_args = {}):
        Type(SymbolType::StructType, facet, definition->parent_scope(),
             definition->name(), ScopeArg::None,
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
    std::span<SymRef<> const> generic_args() const { return _generic_args; }

private:
    std::string make_name() const;
    void verify() const;

    StructDef* _definition;
    utl::small_vector<SymRef<>, 3> _generic_args;
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

// MARK: Traits

/// Base class of all traits
class Trait: public Symbol {
protected:
    Trait(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
          std::string name, ScopeArg scope_arg):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg) {}
};

/// Instantiation of a trait definition
class TraitInst final: public Trait {
public:
    template <RangeOf<SymRef<>> GenArgs = std::array<SymRef<>, 0>>
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
    /// structs
    std::span<SymRef<> const> generic_args() const { return _generic_args; }

private:
    std::string make_name() const;
    void verify() const;

    TraitDef* _definition;
    utl::small_vector<SymRef<>, 3> _generic_args;
};

// MARK: Values

/// Base class of all values
class Value: public Symbol {
public:
    /// The type of this value
    SymRef<Type const> type() const { return _type; }

protected:
    Value(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
          std::string name, ScopeArg scope_arg, SymRef<Type const> type):
        Symbol(sym_type, facet, parent_scope, std::move(name), scope_arg),
        _type(type) {}

private:
    SymRef<Type const> _type;
};

/// Non-type generic parameter
class GenValueParam final: public Value {
public:
    explicit GenValueParam(Facet const* facet, Scope* parent_scope,
                           std::string name, SymRef<Type const> type):
        Value(SymbolType::GenValueParam, facet, parent_scope, std::move(name),
              ScopeArg::None, type) {}
};

} // namespace prism

#undef FACET_TYPE

#endif // PRISM_SEMA2_SYMBOL_H
