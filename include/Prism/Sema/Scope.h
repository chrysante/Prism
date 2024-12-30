#ifndef PRISM_SEMA_SCOPE_H
#define PRISM_SEMA_SCOPE_H

#include <bit>
#include <span>
#include <utility>
#include <vector>

#include <utl/hashtable.hpp>
#include <utl/metric_table.hpp>
#include <utl/small_ptr_vector.hpp>
#include <utl/vector.hpp>

namespace prism {

class Symbol;

namespace detail {
class AssocScope;
}

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

private:
    friend class Symbol;
    friend class detail::AssocScope;

    void addSymbol(Symbol& symbol);

    Scope* _parentScope = nullptr;
    Symbol* _assocSymbol = nullptr;
    std::vector<Symbol*> _symbols;
    utl::hashmap<std::string_view, utl::small_ptr_vector<Symbol*>> _names;
    utl::metric_map<std::string_view, utl::small_ptr_vector<Symbol*>>
        _approxNames;
};

} // namespace prism

#endif // PRISM_SEMA_SCOPE_H
