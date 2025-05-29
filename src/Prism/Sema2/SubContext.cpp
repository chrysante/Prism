#include "Prism/Sema2/SubContext.h"

#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::reverse;
using ranges::views::transform;
using ranges::views::zip;

GenParamBase const* as_gen_param_base(Symbol const* symbol) {
    if (auto* type = dyncast<GenTypeParam const*>(symbol)) return type;
    if (auto* value = dyncast<GenValueParam const*>(symbol)) return value;
    return nullptr;
}

Symbol const* SubContext::resolve(Symbol const* symbol) const {
    if (auto* gen_param = as_gen_param_base(symbol))
        return _stack[gen_param->nesting_depth()][gen_param->index()];
    return symbol;
}

Type const* SubContext::resolve(Type const* type) {
    return cast<Type const*>(resolve(static_cast<Symbol const*>(type)));
}

Value* SubContext::resolve(Value* value) {
    return cast<Value*>(resolve(static_cast<Symbol*>(value)));
}

Value const* SubContext::resolve(Value const* value) {
    return cast<Value const*>(resolve(static_cast<Symbol const*>(value)));
}
