#ifndef PRISM_SEMA_SEMACONTEXT_H
#define PRISM_SEMA_SEMACONTEXT_H

#include <concepts>
#include <memory>
#include <vector>

#include <utl/function_view.hpp>
#include <utl/pimpl.hpp>

#include <Prism/Sema/QualType.h>
#include <Prism/Sema/Scope.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SourceContext;
class Facet;
class SourceFileFacet;

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
            mapSourceToContext(owner->facet(), &owner->sourceContext());
        return cast<Sym*>(addSymbol(std::move(owner)));
    }

    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, SemaContext&, Args...>
    Sym* make(Args&&... args) {
        return make<Sym>(*this, std::forward<Args>(args)...);
    }

    template <std::derived_from<Symbol> Sym, typename... Args>
    Sym* makeBuiltin(BuiltinSymbol builtinID, Args&&... args)
        requires requires { make<Sym>(std::forward<Args>(args)...); }
    {
        auto* sym = make<Sym>(std::forward<Args>(args)...);
        assignBuiltin(builtinID, sym);
        return sym;
    }

    Symbol* getBuiltin(BuiltinSymbol builtin) const;

#define SEMA_BUILTIN(Name, Spelling, SymType) SymType* get##Name() const;
#include <Prism/Sema/Builtins.def>

    ReferenceType const* getRefType(QualType referred);

    DynTraitType const* getDynTraitType(Trait* trait);

    template <std::derived_from<Symbol> S, std::derived_from<GenericSymbol> G>
    std::pair<S*, bool> getGenericInst(G* generic,
                                       std::span<Symbol* const> args) {
        bool existedBefore = true;
        auto* s = getGenInstImpl(generic, args, [&] {
            existedBefore = false;
            return make<S>(generic, utl::small_vector<Symbol*>(args.begin(),
                                                               args.end()));
        });
        return { cast<S*>(s), !existedBefore };
    }

    Scope* makeScope(Scope* parent);

    /// \Returns the source context of \p facet
    SourceContext const* getSourceContext(Facet const* facet) const;

private:
    Symbol* addSymbol(csp::unique_ptr<Symbol> symbol);

    Symbol* assignBuiltin(BuiltinSymbol builtinID, Symbol* symbol);

    Symbol* getGenInstImpl(GenericSymbol* gen, std::span<Symbol* const> args,
                           utl::function_view<Symbol*()> factory);

    void mapSourceToContext(SourceFileFacet const* facet,
                            SourceContext const* ctx);

    struct Impl;

    utl::local_pimpl<Impl, 296> impl;
};

} // namespace prism

#endif // PRISM_SEMA_SEMACONTEXT_H
