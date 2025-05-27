#ifndef PRISM_SEMA2_SEMACONTEXT_H
#define PRISM_SEMA2_SEMACONTEXT_H

#include <concepts>
#include <memory>
#include <vector>

#include <utl/function_view.hpp>
#include <utl/pimpl.hpp>

#include <Prism/Sema2/Scope.h>
#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SourceContext;
class Facet;
class SourceFileFacet;
class FuncSig;

/// Context class that owns and uniques most symbols
class SemaContext {
public:
    SemaContext();
    SemaContext(SemaContext const&) = delete;
    SemaContext& operator=(SemaContext const&) = delete;
    ~SemaContext();

    /// Construct a new module. Must only be called once on the context.
    Module* make_module();

    /// Construct a symbol of type \p Sym
    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, Args...>
    Sym* make(Args&&... args) {
        auto owner = csp::make_unique<Sym>(std::forward<Args>(args)...);
        if (auto* parent_scope = owner->parent_scope())
            parent_scope->add_symbol(*owner);
        if constexpr (std::is_same_v<Sym, SourceFile>)
            map_source_to_context(owner->facet(), &owner->source_context());
        return cast<Sym*>(add_symbol(std::move(owner)));
    }

    /// \overload
    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, SemaContext&, Args...>
    Sym* make(Args&&... args) {
        return make<Sym>(*this, std::forward<Args>(args)...);
    }

    /// Creates a new empty scope
    template <typename... Args>
    Scope* make_scope(Args&&... args) {
        return add_scope(std::make_unique<Scope>(std::forward<Args>(args)...));
    }

    /// \Returns the source context of \p facet
    SourceContext const* get_source_context(Facet const* facet) const;

    /// \Returns the uniqued instantiation of the generic struct \p definition
    /// with arguments \p generic_args
    StructInst* get_struct_instantiation(StructDef* definition,
                                         std::span<Symbol* const> generic_args);

    /// See `get_struct_instantiation()`
    TraitInst* get_trait_instantiation(TraitDef* definition,
                                       std::span<Symbol* const> generic_args);

    /// See `get_struct_instantiation()`
    FunctionInst* get_function_instantiation(
        FunctionDef* definition, std::span<Symbol* const> generic_args);

    /// The function type instance with \p signature
    FunctionType const* get_function_type(FuncSig const& signature);

    /// \Returns the unique generic parameter modulo alpha equivalence.
    /// This means that for every triplet of
    /// \p trait_bound, \p parameter_index, \p generic_nesting_depth
    /// there is exactly one `GenTypeParam` instance. This
    /// instance will be added to each \p parent_scope by the name \p name for
    /// name lookup.
    /// https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence
    GenTypeParam* get_gen_type_param(Facet const* facet, Scope* parent_scope,
                                     std::string name, Trait const* trait_bound,
                                     size_t parameter_index,
                                     size_t generic_nesting_depth);

    /// See `get_gen_type_param()`
    GenValueParam* get_gen_value_param(Facet const* facet, Scope* parent_scope,
                                       std::string name, Type const* type,
                                       size_t parameter_index,
                                       size_t generic_nesting_depth);

    /// The integer literal with value \p value
    /// Uniqued for each facet
    IntLiteral* get_int_literal(Facet const* facet, APInt value,
                                bool is_signed);

    /// # Builtins

    /// \Returns the `type` trait, i.e., the trait matching all types
    BuiltinTrait* get_type_trait() const;

    /// Builtin types @{
    BuiltinType* get_void_type() const;
    BuiltinType* get_bool_type() const;
    BuiltinType* get_byte_type() const;
    BuiltinType* get_i8_type() const;
    BuiltinType* get_i16_type() const;
    BuiltinType* get_i32_type() const;
    BuiltinType* get_i64_type() const;
    BuiltinType* get_u8_type() const;
    BuiltinType* get_u16_type() const;
    BuiltinType* get_u32_type() const;
    BuiltinType* get_u64_type() const;
    BuiltinType* get_f32_type() const;
    BuiltinType* get_f64_type() const;
    /// @}

private:
    Symbol* add_symbol(csp::unique_ptr<Symbol> symbol);
    Scope* add_scope(std::unique_ptr<Scope> scope);
    void map_source_to_context(SourceFileFacet const* facet,
                               SourceContext const* ctx);

    struct Impl;

    utl::local_pimpl<Impl, 1024> impl;
};

} // namespace prism

#endif // PRISM_SEMA2_SEMACONTEXT_H
