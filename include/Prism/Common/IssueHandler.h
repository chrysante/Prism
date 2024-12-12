#ifndef PRISM_COMMON_ISSUEHANDLER_H
#define PRISM_COMMON_ISSUEHANDLER_H

#include <concepts>
#include <iosfwd>
#include <memory>
#include <ranges>
#include <vector>

#include <Prism/Common/Functional.h>
#include <Prism/Common/Issue.h>

namespace prism {

class SourceContext;

class IssueHandler {
    auto view() const { return list | std::views::transform(Dereference); }

public:
    template <std::derived_from<Issue> I, typename... Args>
        requires std::constructible_from<I, Args&&...>
    void push(Args&&... args) {
        push(std::make_unique<I>(std::forward<Args>(args)...));
    }

    void push(std::unique_ptr<Issue> issue) {
        list.push_back(std::move(issue));
    }

    bool empty() const { return list.empty(); }

    size_t size() const { return list.size(); }

    void clear() { list.clear(); }

    auto begin() const { return view().begin(); }
    auto end() const { return view().end(); }
    auto const& front() const { return view().front(); }
    auto const& back() const { return view().back(); }

    /// Writes all issues to `std::cerr`
    void print(SourceContext const& ctx);

    /// Writes all issues formatted to \p os
    void format(std::ostream& os, SourceContext const& ctx) const;

private:
    std::vector<std::unique_ptr<Issue>> list;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUEHANDLER_H
