#ifndef PRISM_SEMA2_SYMREF_H
#define PRISM_SEMA2_SYMREF_H

#include <concepts>
#include <cstdint>
#include <cstring>

#include <utl/ipp.hpp>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SubContext;

struct SubContextIndex {
    uint32_t nesting_index : 8;
    uint32_t param_index   : 24;
};

static_assert(sizeof(SubContextIndex) == 4);

template <std::derived_from<Symbol> S = Symbol>
class SymRef {
public:
    SymRef(S* ptr): u(PtrType(ptr, 0)) {}

    explicit SymRef(SubContextIndex index): u(IndexType{ 1, index }) {}

    template <std::convertible_to<S*> T>
    SymRef(SymRef<T> other):
        u(other.have_ptr() ? U{ other.u.ptr } : U{ other.u.index }) {}

    S* eval(SubContext const& subCtx) const;

private:
    using PtrType = utl::ipp<S*, uint32_t, 1>;

    struct IndexType {
        uint32_t discr;
        SubContextIndex index;
    };

    bool have_ptr() const {
        uint64_t value;
        std::memcpy(&value, &u, sizeof u);
        return (value & 1u) == 0;
    }

    union U {
        PtrType ptr;
        IndexType index;
    } u;
};

template <std::derived_from<Symbol> S>
S* SymRef<S>::eval(SubContext const& subCtx) const {
    if (have_ptr()) return u.ptr.pointer();
    auto* result = lookup_incomplete(subCtx, u.index.index);
    if constexpr (std::same_as<std::remove_cv_t<S>, Symbol>)
        return result;
    else
        return cast<S*>(result);
}

} // namespace prism

#endif // PRISM_SEMA2_SYMREF_H
