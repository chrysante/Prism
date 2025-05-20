#ifndef PRISM_SEMA_EXPRANALYSIS_H
#define PRISM_SEMA_EXPRANALYSIS_H

#include <concepts>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/Symbol.h>

namespace prism {

class Facet;
class AnalysisBase;
class Scope;

namespace detail {

template <std::derived_from<Symbol> S>
S* verifySymbolType(AnalysisBase const&, Facet const*, Symbol*);

}

struct ExprAnalysisOptions {
    bool instantiateGenericsLazily = false;
};

/// Analyzes the parse tree facet \p facet and returns the resolved symbol if
/// possible
///
/// Resulting diagnostics are emitted to the diagnostic emitter of \p context
///
/// \param context The analysis context
/// \param scope The lexical scope in which this facet appears
/// \param facet The parse tree node to analyze
Symbol* analyzeFacet(AnalysisBase const& context, Scope* scope,
                     Facet const* facet, ExprAnalysisOptions options = {});

/// Analyzes the parse tree facet by calling `analyzeFacet()` and verifies that
/// is has symbol type \p S
template <std::derived_from<Symbol> S>
S* analyzeFacetAs(AnalysisBase const& context, Scope* scope, Facet const* facet,
                  ExprAnalysisOptions options = {}) {
    auto* symbol = analyzeFacet(context, scope, facet, options);
    return detail::verifySymbolType<S>(context, facet, symbol);
}

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

} // namespace detail

template <std::derived_from<Symbol> S>
S* detail::verifySymbolType(AnalysisBase const& context, Facet const* facet,
                            Symbol* symbol) {
    auto* result = SymbolConverter<S>::convert(symbol);
    if (result) return result;
    auto typeId = csp::type_to_id_v<std::remove_cv_t<S>>;
    pushBadSymRef(context, facet, symbol, typeId);
    return nullptr;
}

} // namespace prism

#endif // PRISM_SEMA_EXPRANALYSIS_H
