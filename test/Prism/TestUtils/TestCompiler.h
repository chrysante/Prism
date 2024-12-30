#include <string>

#include <range/v3/algorithm.hpp>

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
        assert(false);
        return nullptr;
    }

    template <std::derived_from<Issue> I>
    I const* find() {
        return findImpl<I>(Isa<I>);
    }

private:
    template <typename T>
    static constexpr auto Isa =
        [](auto& p) { return dynamic_cast<T const*>(&p) != nullptr; };

    template <std::derived_from<Issue> I>
    I const* findImpl(auto condition) {
        auto& iss = invocation.getIssueHandler();
        auto itr = ranges::find_if(iss, condition);
        return itr != iss.end() ? dynamic_cast<I const*>(&*itr) : nullptr;
    }

    Invocation invocation;
};

} // namespace prism
