#ifndef PRISM_SEMA2_SUBCONTEXT_H
#define PRISM_SEMA2_SUBCONTEXT_H

#include <span>

#include <utl/stack.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

/// 'Global' substitution context for nested declarations
class SubContext {
public:
    /// Push a declaration context onto the stack
    void push(std::span<Symbol* const> gen_args) {
        _stack.emplace(gen_args.begin(), gen_args.end());
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
    utl::stack<utl::small_vector<Symbol*, 3>, 2> _stack;
};

} // namespace prism

#endif // PRISM_SEMA2_SUBCONTEXT_H
