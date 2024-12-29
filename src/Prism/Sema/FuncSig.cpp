#include "Prism/Sema/FuncSig.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/hash.hpp>

#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::transform;

FuncSig FuncSig::Compute(Type const* ret,
                         std::span<FuncParam const* const> params) {
    return FuncSig(ret,
                   params | transform(FN1(, _1->type())) | ToSmallVector<>);
}

static size_t hashImpl(Type const* ret, std::span<Type const* const> params) {
    size_t seed = 0;
    utl::hash_combine_seed(seed, ret);
    for (auto* type: params)
        utl::hash_combine_seed(seed, type);
    return seed;
}

size_t FuncSig::hashValue() const { return hashImpl(retType(), paramTypes()); }

size_t FuncSig::hashValueIgnoringFirst() const {
    return hashImpl(retType(), paramTypes().subspan(1));
}

bool FuncSig::compareEqIgnoringFirst(FuncSig const& rhs) const {
    return retType() == rhs.retType() &&
           ranges::equal(paramTypes().subspan(1), rhs.paramTypes().subspan(1));
}
