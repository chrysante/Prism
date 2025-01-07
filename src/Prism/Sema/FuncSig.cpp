#include "Prism/Sema/FuncSig.h"

#include <range/v3/view.hpp>

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
