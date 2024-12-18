#ifndef PRISM_SEMA_SCOPE_H
#define PRISM_SEMA_SCOPE_H

#include <span>

#include <utl/vector.hpp>

namespace prism {

class Symbol;

namespace detail {
class AssocScope;
}

class Scope {
public:
    Symbol* assocSymbol() { return _assocSymbol; }

    Symbol const* assocSymbol() const { return _assocSymbol; }

    std::span<Symbol* const> symbols() { return _symbols; }

    std::span<Symbol const* const> symbols() const { return _symbols; }

    void addSymbol(Symbol* symbol) { _symbols.push_back(symbol); }

private:
    friend class detail::AssocScope;

    Symbol* _assocSymbol = nullptr;
    utl::small_vector<Symbol*> _symbols;
};

} // namespace prism

#endif // PRISM_SEMA_SCOPE_H
