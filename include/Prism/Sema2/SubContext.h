#ifndef PRISM_SEMA2_SUBCONTEXT_H
#define PRISM_SEMA2_SUBCONTEXT_H

#include <span>

#include <utl/stack.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema2/SymRef.h>

namespace prism {

class SubContext {
public:
    Symbol* lookup(SubContextIndex idx) const {
        auto ref = table.container()[idx.nesting_index][idx.param_index];
        return ref.eval(*this);
    }

    void push(std::span<Symbol* const> params) {
        table.emplace(params.begin(), params.end());
    }

    void pop() { table.pop(); }

private:
    friend Symbol* lookup_incomplete(SubContext const& This,
                                     SubContextIndex idx) {
        return This.lookup(idx);
    }

    utl::stack<utl::small_vector<SymRef<>, 3>, 1> table;
};

} // namespace prism

#endif // PRISM_SEMA2_SUBCONTEXT_H
