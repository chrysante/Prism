#ifndef PRISM_SEMA_FUNCSIG_H
#define PRISM_SEMA_FUNCSIG_H

#include <span>

#include <range/v3/algorithm.hpp>
#include <utl/hash.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

/// Captures return and parameter types of a function for comparing and hashing
class FuncSig {
public:
    FuncSig() = default;

    explicit FuncSig(Type const* ret, utl::small_vector<Type const*> params):
        _ret(ret), _params(std::move(params)) {}

    static FuncSig Compute(Type const* ret,
                           std::span<FuncParam const* const> params);

    /// \Returns the return type
    Type const* retType() const { return _ret; }

    /// \Returns a view over the parameter types
    std::span<Type const* const> paramTypes() const { return _params; }

    /// \Returns a hash value based on the address identities of the return and
    /// parameter types
    template <typename Proj = ranges::identity>
    size_t hashValue(Proj&& proj = {}) const {
        return hashImpl(retType(), paramTypes(), proj);
    }

    /// Like `hashValue()` but ignores the first parameter. This exists to match
    /// overriding functions with different `this` parameter types
    template <typename Proj = ranges::identity>
    size_t hashValueIgnoringFirst(Proj&& proj = {}) const {
        return hashImpl(retType(), paramTypes().subspan(1), proj);
    }

    /// Like `hashValue()` but ignores the return type. This exists to prevent
    /// function redefinitions
    template <typename Proj = ranges::identity>
    size_t hashValueIgnoringRet(Proj&& proj = {}) const {
        return hashImpl(nullptr, paramTypes(), proj);
    }

    ///
    bool operator==(FuncSig const&) const = default;

    ///
    template <typename Cmp = ranges::equal_to, typename Proj = ranges::identity>
    bool compareEq(FuncSig const& rhs, Cmp&& cmp = {}, Proj&& proj = {}) const {
        return cmpImpl(retType(), paramTypes(), rhs.retType(), rhs.paramTypes(),
                       cmp, proj);
    }

    /// See `hashValueIgnoringFirst()`
    template <typename Cmp = ranges::equal_to, typename Proj = ranges::identity>
    bool compareEqIgnoringFirst(FuncSig const& rhs, Cmp&& cmp = {},
                                Proj&& proj = {}) const {
        return cmpImpl(retType(), paramTypes().subspan(1), rhs.retType(),
                       rhs.paramTypes().subspan(1), cmp, proj);
    }

    /// See `hashValueIgnoringFirst()`
    template <typename Cmp = ranges::equal_to, typename Proj = ranges::identity>
    bool compareEqIgnoringRet(FuncSig const& rhs, Cmp&& cmp = {},
                              Proj&& proj = {}) const {
        return cmpImpl(nullptr, paramTypes(), nullptr, rhs.paramTypes(), cmp,
                       proj);
    }

    /// Function object to use with hash tables
    struct HashIgnoringFirst {
        size_t operator()(FuncSig const& fs) const {
            return fs.hashValueIgnoringFirst();
        }
    };

    /// Function object to use with hash tables
    struct HashIgnoringRet {
        size_t operator()(FuncSig const& fs) const {
            return fs.hashValueIgnoringRet();
        }
    };

    /// Function object to use with hash tables
    struct CompareEqIgnoringFirst {
        bool operator()(FuncSig const& a, FuncSig const& b) const {
            return a.compareEqIgnoringFirst(b);
        }
    };

    /// Function object to use with hash tables
    struct CompareEqIgnoringRet {
        bool operator()(FuncSig const& a, FuncSig const& b) const {
            return a.compareEqIgnoringRet(b);
        }
    };

private:
    static size_t hashImpl(Type const* ret, std::span<Type const* const> params,
                           auto&& proj) {
        size_t seed = 0;
        utl::hash_combine_seed(seed, std::invoke(proj, ret));
        for (auto* type: params)
            utl::hash_combine_seed(seed, std::invoke(proj, type));
        return seed;
    }

    static bool cmpImpl(Type const* lhsRet,
                        std::span<Type const* const> lhsParams,
                        Type const* rhsRet,
                        std::span<Type const* const> rhsParams, auto&& cmp,
                        auto&& proj) {
        return std::invoke(cmp, std::invoke(proj, lhsRet),
                           std::invoke(proj, rhsRet)) &&
               ranges::equal(lhsParams, rhsParams, cmp, proj, proj);
    }

    Type const* _ret = nullptr;
    utl::small_vector<Type const*> _params;
};

} // namespace prism

template <>
struct std::hash<prism::FuncSig> {
    size_t operator()(prism::FuncSig const& fs) const { return fs.hashValue(); }
};

#endif // PRISM_SEMA_FUNCSIG_H
