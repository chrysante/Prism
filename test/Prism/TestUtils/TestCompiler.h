#ifndef PRISM_TESTUTILS_TESTCOMPILER_H
#define PRISM_TESTUTILS_TESTCOMPILER_H

#include <string>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Diagnostic.h"
#include "Prism/Common/DiagnosticHandler.h"
#include "Prism/Common/Functional.h"
#include "Prism/Invocation/Invocation.h"

namespace prism {

class DiagnosticChecker {
public:
    static DiagnosticChecker Make(
        std::string source, InvocationStage until = InvocationStage::Sema);

    template <std::derived_from<Diagnostic> D>
    D const* findOnLine(int line) {
        return findImpl<D>(invocation.getDiagnosticHandler(),
                           onLineFn<D>(line));
    }

    template <std::derived_from<Diagnostic> D>
    D const* findOnLine(Diagnostic const& diag, int line) {
        return findImpl<D>(diag.children() |
                               ranges::views::transform(Dereference),
                           onLineFn<D>(line));
    }

    template <std::derived_from<Diagnostic> D>
    D const* find() {
        return findImpl<D>(invocation.getDiagnosticHandler(), Isa<D>);
    }

    template <std::derived_from<Diagnostic> D>
    D const* find(Diagnostic const& diag) {
        return findImpl<D>(diag.children() |
                               ranges::views::transform(Dereference),
                           Isa<D>);
    }

private:
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

    Invocation invocation;
};

} // namespace prism

#endif // PRISM_TESTUTILS_TESTCOMPILER_H
