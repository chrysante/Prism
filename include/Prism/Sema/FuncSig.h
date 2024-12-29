#ifndef PRISM_SEMA_FUNCSIG_H
#define PRISM_SEMA_FUNCSIG_H

#include <span>

#include <utl/vector.hpp>

#include <Prism/Sema/SymbolFwd.h>

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
    size_t hashValue() const;

    /// Like `hashValue()` but ignores the first parameter. This exists to match
    /// overriding functions with different `this` parameter types
    size_t hashValueIgnoringFirst() const;

    ///
    bool operator==(FuncSig const&) const = default;

    /// See `hashValueIgnoringFirst()`
    bool compareEqIgnoringFirst(FuncSig const& rhs) const;

    /// Function object to use with hash tables
    struct HashIgnoringFirst {
        size_t operator()(FuncSig const& fs) const {
            return fs.hashValueIgnoringFirst();
        }
    };

    /// Function object to use with hash tables
    struct CompareEqIgnoringFirst {
        bool operator()(FuncSig const& a, FuncSig const& b) const {
            return a.compareEqIgnoringFirst(b);
        }
    };

private:
    Type const* _ret;
    utl::small_vector<Type const*> _params;
};

} // namespace prism

template <>
struct std::hash<prism::FuncSig> {
    size_t operator()(prism::FuncSig const& fs) const { return fs.hashValue(); }
};

#endif // PRISM_SEMA_FUNCSIG_H
