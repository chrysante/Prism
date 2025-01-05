#ifndef PRISM_SEMA_EXPRANALYSIS_H
#define PRISM_SEMA_EXPRANALYSIS_H

#include <concepts>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/Symbol.h>

namespace prism {

class Facet;
class AnalysisBase;
class Scope;

struct ExprAnalysisOptions {
    bool instantiateGenericsLazily = false;
};

Symbol* analyzeFacet(AnalysisBase const& context, Scope* scope,
                     Facet const* facet, ExprAnalysisOptions options = {});

namespace detail {

template <typename Target>
struct SymbolConverter {
    static Target* convert(Symbol* sym) { return dyncast<Target*>(sym); }
};

template <typename T>
    requires std::derived_from<T, Type>
struct SymbolConverter<T> {
    static T* convert(Symbol* sym) {
        if (!sym) return nullptr;
        return visit(*sym, [](auto& sym) { return convertImpl(sym); });
    }

    static T* convertImpl(Symbol& sym) { return dyncast<T*>(&sym); }

    static T* convertImpl(BaseClass& base) {
        return dyncast<T*>(const_cast<CompositeType*>(base.type()));
    }
};

void pushBadSymRef(AnalysisBase const& context, Facet const* facet,
                   Symbol* symbol, SymbolType expected);

template <std::derived_from<Symbol> S>
S* verifySymbolType(AnalysisBase const& context, Facet const* facet,
                    Symbol* symbol) {
    auto* result = SymbolConverter<S>::convert(symbol);
    if (result) return result;
    pushBadSymRef(context, facet, symbol, csp::impl::TypeToID<S>);
    return nullptr;
}

} // namespace detail

template <std::derived_from<Symbol> S>
S* analyzeFacetAs(AnalysisBase const& context, Scope* scope, Facet const* facet,
                  ExprAnalysisOptions options = {}) {
    auto* symbol = analyzeFacet(context, scope, facet, options);
    if (!symbol) return nullptr;
    return detail::verifySymbolType<S>(context, facet, symbol);
}

} // namespace prism

#endif // PRISM_SEMA_EXPRANALYSIS_H
