#ifndef PRISM_COMMON_RTTI_H
#define PRISM_COMMON_RTTI_H

#include <csp.hpp>

#define PRISM_DEFINE_RTTI(Type, ID, Parent, Corporeality)                      \
    CSP_DEFINE(Type, ID, Parent, Corporeality)

namespace prism {

using csp::cast;
using csp::dyncast;
using csp::isa;
using csp::visit;

} // namespace prism

#endif // PRISM_COMMON_RTTI_H
