#ifndef PRISM_SEMA_GENERICINSTANTIATION_H
#define PRISM_SEMA_GENERICINSTANTIATION_H

#include <concepts>
#include <span>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaContext;

///
Symbol* instantiateGeneric(SemaContext& ctx, GenericSymbol& gen,
                           std::span<Symbol* const> args);

/// \overload
template <std::derived_from<GenericSymbol> G>
G::InstantiationType* instantiateGeneric(SemaContext& ctx, G& gen,
                                         std::span<Symbol* const> args) {
    auto* inst =
        instantiateGeneric(ctx, static_cast<GenericSymbol&>(gen), args);
    return cast<typename G::InstantiationType*>(inst);
}

} // namespace prism

#endif // PRISM_SEMA_GENERICINSTANTIATION_H
