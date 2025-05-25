#ifndef PRISM_SEMA2_SUBCONTEXT_H
#define PRISM_SEMA2_SUBCONTEXT_H

#include <span>

#include <utl/hashtable.hpp>
#include <utl/stack.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

/// Substitution context for a single declaration
class DeclSubContext {
public:
    explicit DeclSubContext(DeclSymbol* declaration,
                            std::span<Symbol* const> generic_arguments);

    DeclSymbol* declaration() const { return _decl; }

private:
    friend class SubContext;

    // 'Unchecked' resolution
    Symbol const* try_resolve(Symbol const* symbol) const;

    DeclSymbol* _decl;
    utl::hashmap<Symbol*, Symbol*> _map;
};

/// 'Global' substitution context for nested declarations
class SubContext {
public:
    /// Push a declaration context onto the stack
    void push(DeclSymbol* declaration,
              std::span<Symbol* const> generic_arguments) {
        _stack.emplace(declaration, generic_arguments);
    }

    /// Pop the last push context
    void pop() { _stack.pop(); }

    /// Resolves the argument \p symbol if it is a generic argument in this
    /// substitution context
    Symbol* resolve(Symbol* symbol) const {
        return const_cast<Symbol*>(resolve(static_cast<Symbol const*>(symbol)));
    }

    /// \overload
    Symbol const* resolve(Symbol const* symbol) const;

    /// \overload
    Type const* resolve(Type const* type);

    /// \overload
    Value* resolve(Value* value);

    /// \overload
    Value const* resolve(Value const* value);

private:
    utl::stack<DeclSubContext> _stack;
};

} // namespace prism

#endif // PRISM_SEMA2_SUBCONTEXT_H
