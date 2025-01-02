#ifndef PRISM_SEMA_SEMACONTEXT_H
#define PRISM_SEMA_SEMACONTEXT_H

#include <concepts>
#include <memory>
#include <vector>

#include <csp.hpp>
#include <utl/hashtable.hpp>

#include <Prism/Sema/QualType.h>
#include <Prism/Sema/Scope.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class SemaContext {
public:
    SemaContext();
    SemaContext(SemaContext const&) = delete;
    ~SemaContext();

    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, Args...>
    Sym* make(Args&&... args) {
        auto* s = addSymbol(csp::make_unique<Sym>(std::forward<Args>(args)...));
        return cast<Sym*>(s);
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
        builtins[(size_t)builtinID] = sym;
        return sym;
    }

    Symbol* getBuiltin(BuiltinSymbol builtin) const {
        return builtins[(size_t)builtin];
    }

#define SEMA_BUILTIN(Name, Spelling, SymType) SymType* get##Name() const;
#include <Prism/Sema/Builtins.def>

    ReferenceType const* getRefType(QualType referred);

    DynTraitType const* getDynTraitType(Trait* trait);

    template <std::same_as<Scope> S>
    Scope* make(Scope* parent) {
        scopeBag.push_back(std::make_unique<S>(parent));
        return scopeBag.back().get();
    }

private:
    Symbol* addSymbol(csp::unique_ptr<Symbol> symbol);

    std::vector<Symbol*> builtins;
    utl::hashmap<uintptr_t, ReferenceType*> refTypes;
    utl::hashmap<Trait*, DynTraitType*> dynTraitTypes;
    std::vector<csp::unique_ptr<Symbol>> symbolBag;
    std::vector<std::unique_ptr<Scope>> scopeBag;
};

} // namespace prism

#endif // PRISM_SEMA_SEMACONTEXT_H
