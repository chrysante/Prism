#include "Prism/Sema2/AnalysisContext.h"

#include <range/v3/algorithm.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaDiagnostic.h"
#include "Prism/Sema2/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

std::string prism::get_name(Facet const* name_facet,
                            SourceContext const& source_context) {
    if (!name_facet) return {};
    auto* term = cast<TerminalFacet const*>(name_facet);
    return std::string(source_context.getTokenStr(term->token()));
}

Facet const* prism::get_facet(Symbol const& symbol, Scope const* scope) {
    if (auto* facet = symbol.facet()) return facet;
    for (; scope; scope = scope->parent_scope())
        if (auto* facet = scope->get_facet(&symbol)) return facet;
    PRISM_UNREACHABLE();
}

static bool is_function_like(Symbol const* symbol) {
    return isa<Function>(symbol) || isa<FunctionDef>(symbol);
}

bool prism::check_redefinition(DiagnosticEmitter& DE, Scope const* parent_scope,
                               Facet const& facet, std::string_view name,
                               bool for_function) {
    auto existing = parent_scope->symbols_by_name(name);
    if (existing.empty()) return true;
    auto* conflict = existing.front();
    if (for_function) {
        if (ranges::all_of(existing, is_function_like)) return true;
        conflict = *ranges::find_if(existing, FN1(, !is_function_like(_1)));
    }
    DE.emit<Redefinition>(&facet, std::string(name), conflict, parent_scope);
    return false;
}
