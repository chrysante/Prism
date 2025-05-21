#ifndef PRISM_SEMA2_SEMAFWD_H
#define PRISM_SEMA2_SEMAFWD_H

#include <utl/ptr_union.hpp>

#include <Prism/Sema2/SymbolFwd.inl>

namespace prism {

class Scope;
class SemaContext;

class GenValueParam;
using GenericParam = utl::ptr_union<GenTypeParam*, GenValueParam*>;

} // namespace prism

#endif // PRISM_SEMA2_SEMAFWD_H
