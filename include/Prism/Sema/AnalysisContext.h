#ifndef PRISM_SEMA_ANALYSISCONTEXT_H
#define PRISM_SEMA_ANALYSISCONTEXT_H

#include <string>
#include <string_view>

#include <Prism/Common/Assert.h>
#include <Prism/Diagnostic/DiagnosticEmitter.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SourceContext;
class Facet;

/// Returns the facet of \p symbol in \p scope
/// This function exists because not all symbols are unique to their source
/// location. Generic parameters, literals etc. are shared between scope,
/// for these symbols we maintain maps to lookup their facets in a given scope.
Facet const* get_facet(Symbol const& symbol, Scope const* scope);

///
std::string get_name(Facet const* name_facet,
                     SourceContext const* source_context = nullptr);

/// Checks if \p name is already declared in \p scope and generates a diagnostic
/// if so.
/// \Returns true if the name has not been declared yet.
bool check_redefinition(DiagnosticEmitter& DE, Scope const* scope,
                        Facet const& facet, std::string_view name,
                        bool for_function = false);

/// Strips `TypeAliasInst` into its aliased type if possible.
Type* canonicalize(Type* type);

/// \overload
Symbol* canonicalize(Symbol* symbol);

/// \overload
inline Type const* canonicalize(Type const* type) {
    return canonicalize(const_cast<Type*>(type));
}

/// \overload
inline Symbol const* canonicalize(Symbol const* symbol) {
    return canonicalize(const_cast<Symbol*>(symbol));
}

/// Base class for semantic analysis contexts that provides commonly required
/// members
class AnalysisContext {
public:
    SemaContext& ctx;
    DiagnosticEmitter& DE;
    SourceContext const* source_context = nullptr;

    /// See global `get_name()`
    std::string get_name(Facet const* name_facet) const {
        return prism::get_name(name_facet, source_context);
    }

    /// See global `check_redefinition()`
    bool check_redefinition(Scope const* parent_scope, Facet const& facet,
                            std::string_view name, bool for_function = false) {
        return prism::check_redefinition(DE, parent_scope, facet, name,
                                         for_function);
    }
};

} // namespace prism

#endif // PRISM_SEMA_ANALYSISCONTEXT_H
