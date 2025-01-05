#ifndef PRISM_SEMA_GENERICINSTANTIATION_H
#define PRISM_SEMA_GENERICINSTANTIATION_H

#include <concepts>
#include <span>

#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaContext;
class Facet;

///
Symbol* instantiateGeneric(SemaContext& ctx, DiagnosticEmitter& DE,
                           GenericSymbol& gen, Facet const* callFacet,
                           std::span<Symbol* const> args,
                           std::span<Facet const* const> argFacets);

/// \overload
template <std::derived_from<GenericSymbol> G>
G::InstantiationType* instantiateGeneric(
    SemaContext& ctx, DiagnosticEmitter& DE, G& gen, Facet const* callFacet,
    std::span<Symbol* const> args, std::span<Facet const* const> argFacets) {
    auto* inst = instantiateGeneric(ctx, DE, static_cast<GenericSymbol&>(gen),
                                    callFacet, args, argFacets);
    return cast<typename G::InstantiationType*>(inst);
}

/// Instantiates \p gen with \p args but traps on failure
Symbol* instantiateGenericNoFail(SemaContext& ctx, GenericSymbol& gen,
                                 std::span<Symbol* const> args);

/// \overload
template <std::derived_from<GenericSymbol> G>
G::InstantiationType* instantiateGenericNoFail(SemaContext& ctx, G& gen,
                                               std::span<Symbol* const> args) {
    auto* inst =
        instantiateGenericNoFail(ctx, static_cast<GenericSymbol&>(gen), args);
    return cast<typename G::InstantiationType*>(inst);
}

/// Recursively replaces function argument/return types and generic arguments of
/// \p symbol listed in \p genParams with the corresponding argument in
/// \p genArgs
///
/// \Param symbol The symbol to be mapped.
/// \Param genArgs The arguments to replace the generic parameters.
/// \Param genParams The parameters to be replaced in the symbol.
/// \Returns A pointer to the mapped symbol
Symbol* mapInstantiation(SemaContext& ctx, Symbol* symbol,
                         std::span<Symbol* const> genArgs,
                         std::span<Symbol* const> genParams);

/// \overload
template <std::derived_from<Symbol> S>
S* mapInstantiation(SemaContext& ctx, S* symbol,
                    std::span<Symbol* const> genArgs,
                    std::span<Symbol* const> genParams) {
    Symbol* result =
        mapInstantiation(ctx, static_cast<Symbol*>(symbol), genArgs, genParams);
    return cast<S*>(result);
}

} // namespace prism

#endif // PRISM_SEMA_GENERICINSTANTIATION_H
