#ifndef PRISM_SEMA2_FUNCSIG_H
#define PRISM_SEMA2_FUNCSIG_H

#include <span>

#include <Prism/Sema2/SemaFwd.h>

#include <utl/ipp.hpp>
#include <utl/vector.hpp>

namespace prism {

///
class FuncArgSpec {
public:
    FuncArgSpec(PassingConvention passing_convention, Type const* type):
        _value(type, passing_convention) {}

    PassingConvention passing_convention() const { return _value.integer(); }

    Type const* type() const { return _value.pointer(); }

private:
    utl::ipp<Type const*, PassingConvention, 2> _value;
};

///
class FuncSig {
public:
    explicit FuncSig(std::span<FuncArgSpec const> args,
                     Type const* return_type):
        _arguments(args.begin(), args.end()), _return_type(return_type) {}

    ///
    std::span<FuncArgSpec const> arguments() const { return _arguments; }

    ///
    Type const* return_type() const { return _return_type; }

private:
    utl::small_vector<FuncArgSpec, 3> _arguments;
    Type const* _return_type;
};

} // namespace prism

#endif // PRISM_SEMA2_FUNCSIG_H
