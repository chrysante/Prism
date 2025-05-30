#ifndef PRISM_TESTUTILS_TESTCOMPILER_H
#define PRISM_TESTUTILS_TESTCOMPILER_H

#include <string>
#include <type_traits>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Diagnostic/Diagnostic.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Invocation/Invocation.h"
#include "Prism/Sema2/SemaFwd.h"

namespace prism {

template <typename Inv>
class DiagnosticChecker {
    using InvValue = std::remove_cvref_t<Inv>;

public:
    DiagnosticChecker() = default;

    template <std::derived_from<Diagnostic> D>
    D const* find_diag_on_line(int line) const {
        return find_impl<D>(inv.get_diagnostic_emitter().getAll(),
                            on_line_fn<D>(line));
    }

    template <std::derived_from<Diagnostic> D>
    D const* find_diag_on_line(Diagnostic const& diag, int line) const {
        return find_impl<D>(diag.children(), on_line_fn<D>(line));
    }

    template <std::derived_from<Diagnostic> D>
    D const* find_diag() const {
        return find_impl<D>(inv.get_diagnostic_emitter().getAll(), Isa<D>);
    }

    template <std::derived_from<Diagnostic> D>
    D const* find_diag(Diagnostic const& diag) const {
        return find_impl<D>(diag.children(), Isa<D>);
    }

    bool no_diag_on_line(int line) const {
        return find_impl<Diagnostic>(inv.get_diagnostic_emitter().getAll(),
                                     on_line_fn<Diagnostic>(line)) == nullptr;
    }

    InvValue& invocation() { return inv; }

private:
    friend class InvocationTester;

    template <typename I>
    DiagnosticChecker(I&& inv): inv(std::forward<I>(inv)) {}

    template <typename T>
    static constexpr auto Isa =
        [](auto* p) { return dynamic_cast<T const*>(p) != nullptr; };

    template <typename D>
    static auto on_line_fn(int line) {
        PRISM_ASSERT(line > 0);
        return [=](auto* diag) {
            if (!Isa<D>(diag)) return false;
            auto range = diag->sourceRange();
            return range && range->begin.line + 1 == (uint32_t)line;
        };
    }

    template <std::derived_from<Diagnostic> D>
    D const* find_impl(auto&& rng, auto condition) const {
        auto itr = ranges::find_if(rng, condition, ToAddress);
        return itr != ranges::end(rng) ?
                   dynamic_cast<D const*>(std::to_address(*itr)) :
                   nullptr;
    }

    Inv inv;
};

DiagnosticChecker<Invocation> make_diag_checker(
    std::string source, InvocationStage until = InvocationStage::Sema);

namespace detail {

struct InvHolder {
    Invocation inv;
};

} // namespace detail

class InvocationTester:
    private detail::InvHolder,
    public DiagnosticChecker<Invocation&> {
public:
    InvocationTester(): DiagnosticChecker(InvHolder::inv) {}

    Invocation& invocation() { return InvHolder::inv; }

    ///
    Symbol* eval(std::string_view expr_source);

    ///
    Symbol* eval(Scope* scope, std::string_view expr_source);

    /// \overload
    template <std::derived_from<Symbol> S>
    S* eval(std::string_view expr_source) {
        auto* sym = eval(expr_source);
        return dyncast<S*>(sym);
    }

    /// \overload
    template <std::derived_from<Symbol> S>
    S* eval(Scope* scope, std::string_view expr_source) {
        auto* sym = eval(scope, expr_source);
        return cast<S*>(sym);
    }
};

struct InvTesterOptions {
    bool expect_no_errors = false;
};

InvocationTester make_inv_tester(std::string source,
                                 InvTesterOptions options = {},
                                 InvocationStage until = InvocationStage::Sema);

} // namespace prism

#endif // PRISM_TESTUTILS_TESTCOMPILER_H
