#ifndef PRISM_SEMA2_SUBCONTEXT_H
#define PRISM_SEMA2_SUBCONTEXT_H

#include <span>

#include <range/v3/view.hpp>
#include <utl/stack.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/Assert.h>
#include <Prism/Sema2/AnalysisContext.h>
#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class GenParamBase;

/// Substitution context for nested declarations
class SubContext {
public:
    /// Push a declaration context onto the stack
    void push(std::span<Symbol* const> gen_args) {
        _stack.emplace(gen_args.begin(), gen_args.end());
        for (auto& sym: top_level())
            sym = canonicalize(sym);
    }

    ///
    void push_empty(size_t num_arguments) {
        _stack.emplace(num_arguments, nullptr);
    }

    /// Pop the top argument list
    void pop() { _stack.pop(); }

    /// Pop the \p count top argument list
    void pop(size_t count) {
        PRISM_ASSERT(depth() >= count);
        _stack.pop(count);
    }

    /// Pop until depth is \p new_depth
    void pop_to(size_t new_depth) {
        PRISM_ASSERT(depth() >= new_depth);
        pop(depth() - new_depth);
    }

    /// \Returns the generic argument substituted for \p gen_param
    Symbol* resolve(GenParamBase const& gen_param) const;

    /// A flat range view over the generic arguments
    auto flat_view() { return _stack | ranges::views::join; }

    /// \overload
    auto flat_view() const { return _stack | ranges::views::join; }

    /// The generic nesting depth
    size_t depth() const { return _stack.size(); }

    /// \Returns a view over the arguments at \p depth_level
    std::span<Symbol* const> level(size_t depth_level) const {
        return _stack[depth_level];
    }

    /// \overload
    std::span<Symbol*> level(size_t level) { return _stack[level]; }

    /// \Returns a view over the arguments at the top (inner most) level
    std::span<Symbol*> top_level() { return level(depth() - 1); }

    /// \overload
    std::span<Symbol* const> top_level() const { return level(depth() - 1); }

    /// True if no argument in the context is null
    bool none_is_null() const;

    ///
    bool any_is_null() const { return !none_is_null(); }

    size_t hash_value() const;

    bool operator==(SubContext const& rhs) const = default;

private:
    utl::stack<utl::small_vector<Symbol*, 3>, 2> _stack;
};

} // namespace prism

#endif // PRISM_SEMA2_SUBCONTEXT_H
