#ifndef PRISM_SEMA_EXPRANALYSIS_H
#define PRISM_SEMA_EXPRANALYSIS_H

#include <concepts>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/Symbol.h>

namespace prism {

class Facet;
class AnalysisBase;
class Scope;

Symbol* analyzeFacet(AnalysisBase const& context, Scope* scope,
                     Facet const* facet);

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
        return dyncast<T*>(const_cast<UserType*>(base.type()));
    }
};

template <std::derived_from<Symbol> S>
S* verifySymbolType(AnalysisBase const& context, Symbol* symbol) {
    auto* result = SymbolConverter<S>::convert(symbol);
    if (result) return result;
    PRISM_UNIMPLEMENTED();
    return nullptr;
}

} // namespace detail

template <std::derived_from<Symbol> S>
S* analyzeFacetAs(AnalysisBase const& context, Scope* scope,
                  Facet const* facet) {
    auto* symbol = analyzeFacet(context, scope, facet);
    if (!symbol) return nullptr;
    return detail::verifySymbolType<S>(context, symbol);
}

} // namespace prism

#endif // PRISM_SEMA_EXPRANALYSIS_H
