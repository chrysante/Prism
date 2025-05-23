#ifndef PRISM_SEMA2_FUNCSIG_H
#define PRISM_SEMA2_FUNCSIG_H

#include <span>

#include <Prism/Sema2/SemaFwd.h>

#include <utl/ipp.hpp>
#include <utl/vector.hpp>

namespace prism {

/// Function argument type for `FuncSig`
class FuncArgSpec {
public:
    FuncArgSpec(PassingConvention passing_convention, Type const* type):
        _value(type, passing_convention) {}

    PassingConvention passing_convention() const { return _value.integer(); }

    Type const* type() const { return _value.pointer(); }

    bool operator==(FuncArgSpec const&) const = default;

private:
    utl::ipp<Type const*, PassingConvention, 2> _value;
};

/// Function signature. Captures type argument types and passing conventions and
/// the return type.
class FuncSig {
public:
    explicit FuncSig(std::span<FuncArgSpec const> args,
                     Type const* return_type):
        _arguments(args.begin(), args.end()), _return_type(return_type) {}

    /// View over the arguments
    std::span<FuncArgSpec const> arguments() const { return _arguments; }

    /// The return type
    Type const* return_type() const { return _return_type; }

    /// Compares type pointers and passing conventions
    bool operator==(FuncSig const& rhs) const;

    /// Hash value for storing function signatures in hash tables
    size_t hash_value() const;

private:
    utl::small_vector<FuncArgSpec, 3> _arguments;
    Type const* _return_type;
};

} // namespace prism

template <>
struct std::hash<prism::FuncSig> {
    size_t operator()(prism::FuncSig const& func_sig) const {
        return func_sig.hash_value();
    }
};

#endif // PRISM_SEMA2_FUNCSIG_H
