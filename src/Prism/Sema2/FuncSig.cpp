#include "Prism/Sema2/FuncSig.h"

#include <bit>

#include <range/v3/algorithm.hpp>
#include <utl/hash.hpp>

using namespace prism;

bool FuncSig::operator==(FuncSig const& rhs) const {
    return return_type() == rhs.return_type() &&
           ranges::equal(arguments(), rhs.arguments());
}

size_t FuncSig::hash_value() const {
    size_t seed = std::hash<Type const*>{}(return_type());
    for (auto arg: arguments())
        utl::hash_combine_seed(seed, std::bit_cast<uint64_t>(arg));
    return seed;
}
