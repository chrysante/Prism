#ifndef PRISM_SEMA_SEMACONTEXT_H
#define PRISM_SEMA_SEMACONTEXT_H

#include <concepts>
#include <memory>
#include <vector>

#include <csp.hpp>

#include <Prism/Sema/Scope.h>

namespace prism {

class Symbol;
class SemaContext {
public:
    SemaContext();
    SemaContext(SemaContext const&) = delete;
    ~SemaContext();

    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, Args...>
    Sym* make(Args&&... args) {
        return cast<Sym*>(
            addSymbol(csp::make_unique<Sym>(std::forward<Args>(args)...)));
    }
    
    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, SemaContext&, Args...>
    Sym* make(Args&&... args) {
        return make(*this, std::forward<Args>(args)...);
    }

    template <std::derived_from<Symbol> Sym, typename... Args>
        requires std::constructible_from<Sym, SemaContext&, Args...>
    Sym* make(Args&&... args) {
        return make(*this, std::forward<Args>(args)...);
    }

    template <std::same_as<Scope> S>
    Scope* make(Scope* parent) {
        scopeBag.push_back(std::make_unique<S>(parent));
        return scopeBag.back().get();
    }

private:
    Symbol* addSymbol(csp::unique_ptr<Symbol> symbol);

    std::vector<csp::unique_ptr<Symbol>> symbolBag;
    std::vector<std::unique_ptr<Scope>> scopeBag;
};

} // namespace prism

#endif // PRISM_SEMA_SEMACONTEXT_H
