#ifndef PRISM_SEMA_FUNCSIG_H
#define PRISM_SEMA_FUNCSIG_H

#include <bit>
#include <span>

#include <utl/hash.hpp>
#include <utl/ipp.hpp>
#include <utl/vector.hpp>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

template <typename ParamType>
class SignatureBase {
public:
    explicit SignatureBase(std::span<ParamType const> args):
        _arguments(args.begin(), args.end()) {}

    /// View over the arguments
    std::span<ParamType const> arguments() const { return _arguments; }

    /// Compares the parameter objects
    bool operator==(SignatureBase const& rhs) const = default;

    /// Hash value for storing function signatures in hash tables
    size_t hash_value() const {
        size_t seed = 0;
        for (auto arg: arguments())
            utl::hash_combine_seed(seed, arg);
        return seed;
    }

private:
    utl::small_vector<ParamType, 3> _arguments;
};

} // namespace prism

template <typename ParamType>
struct std::hash<prism::SignatureBase<ParamType>> {
    size_t operator()(prism::SignatureBase<ParamType> const& sig) const {
        return sig.hash_value();
    }
};

namespace prism {

/// Function argument type for `FuncSig`
class FuncArgSpec {
public:
    FuncArgSpec() = default;

    FuncArgSpec(PassingConvention passing_convention, Type const* type):
        _value(type, passing_convention) {}

    PassingConvention passing_convention() const { return _value.integer(); }

    Type const* type() const { return _value.pointer(); }

    bool operator==(FuncArgSpec const&) const = default;

    size_t hash_value() const { return std::hash<ValueType>{}(_value); }

private:
    using ValueType = utl::ipp<Type const*, PassingConvention, 2>;
    ValueType _value{};
};

} // namespace prism

template <>
struct std::hash<prism::FuncArgSpec> {
    size_t operator()(prism::FuncArgSpec const& arg) const {
        return arg.hash_value();
    }
};

namespace prism {

/// Generic parameter signature
class GenericSignature: public SignatureBase<Symbol*> {
public:
    using SignatureBase::arguments;
    using SignatureBase::hash_value;
    using SignatureBase::SignatureBase;
    bool operator==(GenericSignature const&) const = default;
};

/// Function signature. Captures type argument types and passing conventions and
/// the return type.
class FuncSig: public SignatureBase<FuncArgSpec> {
public:
    explicit FuncSig(std::span<FuncArgSpec const> args,
                     Type const* return_type):
        SignatureBase(args), _return_type(return_type) {}

    /// View over the arguments
    using SignatureBase::arguments;

    /// The return type
    Type const* return_type() const { return _return_type; }

    /// Compares type pointers and passing conventions
    bool operator==(FuncSig const& rhs) const = default;

    /// Hash value for storing function signatures in hash tables
    size_t hash_value() const {
        return utl::hash_combine(static_cast<SignatureBase const&>(*this),
                                 _return_type);
    }

    ///
    bool compare_ignoring_return_type(FuncSig const& rhs) const {
        return static_cast<SignatureBase const&>(*this) == rhs;
    }

    ///
    size_t hash_value_ignoring_return_type() const {
        return SignatureBase::hash_value();
    }

    struct CompareIgnoringReturnType {
        bool operator()(FuncSig const& a, FuncSig const& b) const {
            return a.compare_ignoring_return_type(b);
        }
    };

    struct HashIgnoringReturnType {
        bool operator()(FuncSig const& func_sig) const {
            return func_sig.hash_value_ignoring_return_type();
        }
    };

private:
    Type const* _return_type;
};

} // namespace prism

template <>
struct std::hash<prism::GenericSignature> {
    size_t operator()(prism::GenericSignature const& gen_sig) const {
        return gen_sig.hash_value();
    }
};

template <>
struct std::hash<prism::FuncSig> {
    size_t operator()(prism::FuncSig const& func_sig) const {
        return func_sig.hash_value();
    }
};

#endif // PRISM_SEMA_FUNCSIG_H
