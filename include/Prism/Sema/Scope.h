#ifndef PRISM_SEMA_SCOPE_H
#define PRISM_SEMA_SCOPE_H

#include <span>

#include <utl/hashtable.hpp>

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
    std::span<Symbol* const> symbols() { return _symbols.values(); }

    /// \overload
    std::span<Symbol const* const> symbols() const { return _symbols.values(); }

private:
    friend class Symbol;
    friend class detail::AssocScope;

    void addSymbol(Symbol* symbol);

    Scope* _parentScope = nullptr;
    Symbol* _assocSymbol = nullptr;
    utl::hashset<Symbol*> _symbols;
};

} // namespace prism

#endif // PRISM_SEMA_SCOPE_H
