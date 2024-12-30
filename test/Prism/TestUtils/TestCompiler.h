#ifndef PRISM_TESTUTILS_TESTCOMPILER_H
#define PRISM_TESTUTILS_TESTCOMPILER_H

#include <string>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Common/Issue.h"
#include "Prism/Common/IssueHandler.h"
#include "Prism/Invocation/Invocation.h"

namespace prism {

class IssueChecker {
public:
    static IssueChecker Make(std::string source,
                             InvocationStage until = InvocationStage::Sema);

    template <std::derived_from<Issue> I>
    I const* findOnLine(int line) {
        return findImpl<I>(invocation.getIssueHandler(), onLineFn<I>(line));
    }

    template <std::derived_from<Issue> I>
    I const* findOnLine(Issue const& issue, int line) {
        return findImpl<I>(issue.children() |
                               ranges::views::transform(Dereference),
                           onLineFn<I>(line));
    }

    template <std::derived_from<Issue> I>
    I const* find() {
        return findImpl<I>(invocation.getIssueHandler(), Isa<I>);
    }

    template <std::derived_from<Issue> I>
    I const* find(Issue const& issue) {
        return findImpl<I>(issue.children() |
                               ranges::views::transform(Dereference),
                           Isa<I>);
    }

private:
    template <typename T>
    static constexpr auto Isa =
        [](auto& p) { return dynamic_cast<T const*>(&p) != nullptr; };

    template <typename I>
    static auto onLineFn(int line) {
        PRISM_ASSERT(line > 0);
        return [=](auto& issue) {
            if (!Isa<I>(issue)) return false;
            auto range = issue.sourceRange();
            return range && range->begin.line + 1 == (uint32_t)line;
        };
    }

    template <std::derived_from<Issue> I>
    I const* findImpl(auto&& rng, auto condition) {
        auto itr = ranges::find_if(rng, condition);
        return itr != ranges::end(rng) ? dynamic_cast<I const*>(&*itr) :
                                         nullptr;
    }

    Invocation invocation;
};

} // namespace prism

#endif // PRISM_TESTUTILS_TESTCOMPILER_H
