#ifndef PRISM_COMMON_RTTI_H
#define PRISM_COMMON_RTTI_H

#include <csp.hpp>

#define PRISM_DEFINE_RTTI(Type, ID, Parent, Corporeality)                      \
    CSP_DEFINE(Type, ID, Parent, Corporeality)

namespace prism {

using namespace csp::ops;

} // namespace prism

#endif // PRISM_COMMON_RTTI_H
