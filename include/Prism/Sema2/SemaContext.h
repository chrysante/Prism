#ifndef PRISM_SEMA2_SEMACONTEXT_H
#define PRISM_SEMA2_SEMACONTEXT_H

#include <concepts>
#include <memory>
#include <vector>

#include <utl/function_view.hpp>
#include <utl/pimpl.hpp>

#include <Prism/Sema2/Scope.h>
#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class SourceContext;
class Facet;
class SourceFileFacet;

/// Context class that owns and uniques most symbols
class SemaContext {
public:
    SemaContext();
    SemaContext(SemaContext const&) = delete;
    SemaContext& operator=(SemaContext const&) = delete;
    ~SemaContext();

    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, Args...>
    Sym* make(Args&&... args) {
        auto owner = csp::make_unique<Sym>(std::forward<Args>(args)...);
        if constexpr (std::is_same_v<Sym, SourceFile>)
            map_source_to_context(owner->facet(), &owner->source_context());
        return cast<Sym*>(add_symbol(std::move(owner)));
    }

    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, SemaContext&, Args...>
    Sym* make(Args&&... args) {
        return make<Sym>(*this, std::forward<Args>(args)...);
    }

    /// Creates a new empty scope
    Scope* make_scope(Symbol* defining_symbol = nullptr);

    /// \Returns the source context of \p facet
    SourceContext const* get_source_context(Facet const* facet) const;

private:
    Symbol* add_symbol(csp::unique_ptr<Symbol> symbol);

    void map_source_to_context(SourceFileFacet const* facet,
                               SourceContext const* ctx);

    struct Impl;

    utl::local_pimpl<Impl, 296> impl;
};

} // namespace prism

#endif // PRISM_SEMA2_SEMACONTEXT_H
