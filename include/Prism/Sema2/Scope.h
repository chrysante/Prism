#ifndef PRISM_SEMA2_SCOPE_H
#define PRISM_SEMA2_SCOPE_H

#include <bit>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <utl/hashtable.hpp>
#include <utl/metric_table.hpp>
#include <utl/tiny_ptr_vector.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema2/FuncSig.h>
#include <Prism/Sema2/FunctionOverloadMap.h>

namespace prism {

class Symbol;
class ScopeArg;
class SemaContext;

///
class Scope {
public:
    /// Complete constructor
    /// \pre \p def_symbol must not be null
    explicit Scope(Symbol* def_symbol) { set_defining_symbol(def_symbol); }

    /// Partial constructors, must call `set_defining_symbol()` after
    /// construction @{
    Scope() = default;
    explicit Scope(Scope* parent_scope): _parent_scope(parent_scope) {}
    /// @}

    /// \Returns the symbol defining this scope
    Symbol* defining_symbol() { return _defining_symbol; }

    /// \overload
    Symbol const* defining_symbol() const { return _defining_symbol; }

    /// \Returns the parent scope. This is the same as
    /// `assoc_symbol()->parent_scope()`
    Scope* parent_scope() { return _parent_scope; }

    /// \overload
    Scope const* parent_scope() const { return _parent_scope; }

    /// \Returns a list of all symbols in this scope
    std::span<Symbol* const> symbols() { return _symbols; }

    /// \overload
    std::span<Symbol const* const> symbols() const { return _symbols; }

    /// \Returns all symbols named \p name in this scope
    std::span<Symbol* const> symbols_by_name(std::string_view name) {
        return std::bit_cast<std::span<Symbol* const>>(
            std::as_const(*this).symbols_by_name(name));
    }

    /// \overload
    std::span<Symbol const* const> symbols_by_name(std::string_view name) const;

    /// \Returns all symbols named \p name or similarly in this scope
    utl::small_vector<Symbol*> symbols_by_approx_name(std::string_view name);

    /// \overload
    utl::small_vector<Symbol const*> symbols_by_approx_name(
        std::string_view name) const;

    /// \Returns the function named \p name with exact signature \p func_sig or
    /// null if none is found
    FunctionDef* function_by_name_and_sig(std::string_view name,
                                          GenericSignature const& generic_sig,
                                          FuncSig const& function_sig) {
        return const_cast<FunctionDef*>(
            std::as_const(*this).function_by_name_and_sig(name, generic_sig,
                                                          function_sig));
    }

    /// \overload
    FunctionDef const* function_by_name_and_sig(
        std::string_view name, GenericSignature const& generic_sig,
        FuncSig const& function_sig) const;

private:
    friend class SemaContext;
    friend class ScopeArg;
    friend struct NameResolution;

    void add_symbol(Symbol& symbol);
    /// \pre \p def_symbol must not be null
    void set_defining_symbol(Symbol* defining_symbol);

    // To be used by NameResolution
    void set_function_signature(GenericSignature generic_sig,
                                FuncSig function_sig, FunctionDef* function);

    Symbol* _defining_symbol = nullptr;
    Scope* _parent_scope = nullptr;
    std::vector<Symbol*> _symbols;
    utl::hashmap<std::string_view, utl::tiny_ptr_vector<Symbol*>> _names;
    utl::metric_map<std::string_view, utl::tiny_ptr_vector<Symbol*>>
        _approx_names;
    FunctionOverloadMap _function_overload_map;
};

} // namespace prism

#endif // PRISM_SEMA2_SCOPE_H
