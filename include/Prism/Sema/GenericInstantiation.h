#ifndef PRISM_SEMA_GENERICINSTANTIATION_H
#define PRISM_SEMA_GENERICINSTANTIATION_H

#include <span>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaContext;

///
Symbol* instantiateGeneric(SemaContext& ctx, GenericSymbol& gen,
                           std::span<Symbol* const> args);

} // namespace prism

#endif // PRISM_SEMA_GENERICINSTANTIATION_H
