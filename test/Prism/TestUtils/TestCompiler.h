#ifndef PRISM_TESTUTILS_TESTCOMPILER_H
#define PRISM_TESTUTILS_TESTCOMPILER_H

#include <string>
#include <type_traits>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Diagnostic/Diagnostic.h"
#include "Prism/Diagnostic/DiagnosticHandler.h"
#include "Prism/Invocation/Invocation.h"
#include "Prism/Sema/SemaFwd.h"

namespace prism {

template <typename Inv>
class DiagnosticChecker {
    using InvValue = std::remove_cvref_t<Inv>;

public:
    DiagnosticChecker() = default;

    template <std::derived_from<Diagnostic> D>
    D const* findDiagOnLine(int line) {
        return findImpl<D>(inv.getDiagnosticHandler(), onLineFn<D>(line));
    }

    template <std::derived_from<Diagnostic> D>
    D const* findDiagOnLine(Diagnostic const& diag, int line) {
        return findImpl<D>(diag.children() |
                               ranges::views::transform(Dereference),
                           onLineFn<D>(line));
    }

    template <std::derived_from<Diagnostic> D>
    D const* findDiag() {
        return findImpl<D>(inv.getDiagnosticHandler(), Isa<D>);
    }

    template <std::derived_from<Diagnostic> D>
    D const* findDiag(Diagnostic const& diag) {
        return findImpl<D>(diag.children() |
                               ranges::views::transform(Dereference),
                           Isa<D>);
    }

    InvValue& invocation() { return inv; }

private:
    friend class InvocationTester;

    template <typename I>
    DiagnosticChecker(I&& inv): inv(std::forward<I>(inv)) {}

    template <typename T>
    static constexpr auto Isa =
        [](auto& p) { return dynamic_cast<T const*>(&p) != nullptr; };

    template <typename D>
    static auto onLineFn(int line) {
        PRISM_ASSERT(line > 0);
        return [=](auto& diag) {
            if (!Isa<D>(diag)) return false;
            auto range = diag.sourceRange();
            return range && range->begin.line + 1 == (uint32_t)line;
        };
    }

    template <std::derived_from<Diagnostic> D>
    D const* findImpl(auto&& rng, auto condition) {
        auto itr = ranges::find_if(rng, condition);
        return itr != ranges::end(rng) ? dynamic_cast<D const*>(&*itr) :
                                         nullptr;
    }

    Inv inv;
};

DiagnosticChecker<Invocation> makeDiagChecker(
    std::string source, InvocationStage until = InvocationStage::Sema);

namespace detail {

struct InvHolder {
    Invocation inv;
};

} // namespace detail

class InvocationTester: detail::InvHolder, DiagnosticChecker<Invocation&> {
public:
    InvocationTester(): DiagnosticChecker(InvHolder::inv) {}

    Invocation& invocation() { return InvHolder::inv; }

    ///
    Symbol* eval(std::string_view exprSource);

    ///
    Symbol* eval(Scope* scope, std::string_view exprSource);

    /// \overload
    template <std::derived_from<Symbol> S>
    S* eval(std::string_view exprSource) {
        auto* sym = eval(exprSource);
        return cast<S*>(sym);
    }

    /// \overload
    template <std::derived_from<Symbol> S>
    S* eval(Scope* scope, std::string_view exprSource) {
        auto* sym = eval(scope, exprSource);
        return cast<S*>(sym);
    }
};

struct InvTesterOptions {
    bool expectNoErrors = false;
};

InvocationTester makeInvTester(std::string source,
                               InvTesterOptions options = {},
                               InvocationStage until = InvocationStage::Sema);

} // namespace prism

#endif // PRISM_TESTUTILS_TESTCOMPILER_H
