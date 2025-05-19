#ifndef PRISM_SEMA_SCOPE_H
#define PRISM_SEMA_SCOPE_H

#include <bit>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

#include <utl/hashtable.hpp>
#include <utl/metric_table.hpp>
#include <utl/tiny_ptr_vector.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/FuncSig.h>

namespace prism {

class Symbol;
class SemaContext;

namespace detail {
class AssocScope;
Scope* make_scope(SemaContext& ctx, Symbol* This, Scope* parent);
} // namespace detail

///
class Scope {
public:
    explicit Scope(Scope* parent): _parentScope(parent) {}

    /// \Returns the parent scope
    Scope* parent() { return _parentScope; }

    /// \overload
    Scope const* parent() const { return _parentScope; }

    /// \Returns the symbol associated with this scope, like a `FunctionImpl`
    /// for a function scope
    Symbol* assocSymbol() { return _assocSymbol; }

    /// \overload
    Symbol const* assocSymbol() const { return _assocSymbol; }

    /// \Returns a list of all symbols in this scope
    std::span<Symbol* const> symbols() { return _symbols; }

    /// \overload
    std::span<Symbol const* const> symbols() const { return _symbols; }

    /// \Returns all symbols named \p name in this scope
    std::span<Symbol* const> symbolsByName(std::string_view name) {
        return std::bit_cast<std::span<Symbol* const>>(
            std::as_const(*this).symbolsByName(name));
    }

    /// \overload
    std::span<Symbol const* const> symbolsByName(std::string_view name) const;

    /// \Returns all symbols named \p name or similarly in this scope
    utl::small_vector<Symbol*> symbolsByApproxName(std::string_view name);

    /// \overload
    utl::small_vector<Symbol const*> symbolsByApproxName(
        std::string_view name) const;

    /// \Returns the function named \p name with exact signature \p funcSig or null if none is found
    Function* functionByNameAndSig(std::string_view name,
                                   FuncSig const& funcSig) {
        return const_cast<Function*>(
            std::as_const(*this).functionByNameAndSig(name, funcSig));
    }

    /// \overload
    Function const* functionByNameAndSig(std::string_view name,
                                         FuncSig const& funcSig) const;

    ///
    void setFunctionSignature(FuncSig sig, Function* function);

private:
    friend class Symbol;
    friend class detail::AssocScope;

    friend Scope* detail::make_scope(SemaContext&, Symbol*, Scope*);

    void addSymbol(Symbol& symbol);

    Scope* _parentScope = nullptr;
    Symbol* _assocSymbol = nullptr;
    std::vector<Symbol*> _symbols;
    utl::hashmap<std::string_view, utl::tiny_ptr_vector<Symbol*>> _names;
    utl::metric_map<std::string_view, utl::tiny_ptr_vector<Symbol*>>
        _approxNames;
    using OverloadMap =
        utl::hashmap<FuncSig, Function*, FuncSig::HashIgnoringRet,
                     FuncSig::CompareEqIgnoringRet>;

    utl::hashmap<std::string, OverloadMap> _functionMap;
};

} // namespace prism

#endif // PRISM_SEMA_SCOPE_H
