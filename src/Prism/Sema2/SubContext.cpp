#include "Prism/Sema2/SubContext.h"

#include <range/v3/algorithm.hpp>
#include <utl/hash.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

Symbol* SubContext::resolve(GenParamBase const& gen_param) const {
    return _stack[gen_param.nesting_depth()][gen_param.index()];
}

bool SubContext::none_is_null() const {
    return ranges::all_of(flat_view(), FN1(, _1 != nullptr));
}

size_t SubContext::hash_value() const {
    size_t seed = 0;
    for (auto* sym: flat_view())
        utl::hash_combine_seed(seed, sym);
    return seed;
}
