#ifndef PRISM_SEMA_EXPRANALYSIS_H
#define PRISM_SEMA_EXPRANALYSIS_H

#include <concepts>
#include <span>

#include <Prism/Common/Assert.h>
#include <Prism/Sema/AnalysisContext.h>
#include <Prism/Sema/Symbol.h>

namespace prism {

class Facet;
class AnalysisContext;
class Scope;
class FacetAnalysisDelegate;

namespace detail {

template <std::derived_from<Symbol> S>
S* verify_symbol_type(AnalysisContext const&, Facet const*, Symbol*);

}

/// Analyzes the parse tree facet \p facet and returns the resolved symbol if
/// possible
///
/// Resulting diagnostics are emitted to the diagnostic emitter of \p context
///
/// \param context The analysis context
/// \param inst_emitter Interface used to emit instructions
/// \param scope The lexical scope in which this facet appears
/// \param facet The parse tree node to analyze
Symbol* analyze_facet(AnalysisContext const& context,
                      FacetAnalysisDelegate& delegate, SubContext& sub_context,
                      Scope* scope, Facet const* facet);

/// Analyzes the parse tree facet by calling `analyzeFacet()` and verifies that
/// is has symbol type \p S
template <std::derived_from<Symbol> S>
S* analyze_facet_as(AnalysisContext const& context,
                    FacetAnalysisDelegate& delegate, SubContext& sub_context,
                    Scope* scope, Facet const* facet) {
    auto* symbol = analyze_facet(context, delegate, sub_context, scope, facet);
    return detail::verify_symbol_type<S>(context, facet, symbol);
}

/// Interface used by `analyze_facet()` to provide callbacks
class FacetAnalysisDelegate {
public:
    virtual ~FacetAnalysisDelegate() = default;
    virtual void emit_instruction(Instruction& inst) = 0;
    virtual void encounter_callback(Symbol*) {}
};

namespace detail {

template <typename S>
struct SymbolConverter {
    static S* convert(Symbol* sym) { return dyncast<S*>(sym); }
};

#if 0
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
#endif

void push_bad_sym_ref(AnalysisContext const& context, Facet const* facet,
                      Symbol* symbol, SymbolType expected);

} // namespace detail

template <std::derived_from<Symbol> S>
S* detail::verify_symbol_type(AnalysisContext const& context,
                              Facet const* facet, Symbol* symbol) {
    if (!symbol) return nullptr;
    auto* result = SymbolConverter<S>::convert(symbol);
    if (result) return result;
    auto type_id = csp::type_to_id_v<std::remove_cv_t<S>>;
    push_bad_sym_ref(context, facet, symbol, type_id);
    return nullptr;
}

} // namespace prism

#endif // PRISM_SEMA_EXPRANALYSIS_H
