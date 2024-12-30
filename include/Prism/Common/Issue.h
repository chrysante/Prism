#ifndef PRISM_COMMON_ISSUE_H
#define PRISM_COMMON_ISSUE_H

#include <concepts>
#include <cstdint>
#include <functional>
#include <iosfwd>
#include <memory>
#include <optional>
#include <vector>

#include <range/v3/view.hpp>

#include <Prism/Common/Functional.h>
#include <Prism/Source/SourceLocation.h>

namespace prism {

class SourceContext;
class TreeFormatter;

/// Base class of all issues
class Issue {
public:
    enum Kind { Error, Warning, Note, Hint };

    virtual ~Issue() = default;

    /// Formats this issue to \p os
    void format(std::ostream& os, SourceContext const* ctx) const;

    ///
    Kind kind() const { return _kind; }

    /// \Returns the source context in which this diagnostic was generated
    SourceContext const* sourceContext() const { return _sourceContext; }

    /// \Returns the position in the source code where this issue occurred
    std::optional<FullSourceRange> sourceRange() const;

    /// \Returns a view over the child issues
    auto children() const {
        return _children | ranges::views::transform(ToConstAddress);
    }

    /// Adds a child issue
    Issue* addChild(std::unique_ptr<Issue> child) {
        _children.push_back(std::move(child));
        return _children.back().get();
    }

    /// \overload
    template <std::derived_from<Issue> I, typename... Args>
        requires std::constructible_from<I, Args...>
    I* addChild(Args&&... args) {
        return static_cast<I*>(
            addChild(std::make_unique<I>(std::forward<Args>(args)...)));
    }

protected:
    explicit Issue(Kind kind, std::optional<SourceRange> sourceRange,
                   SourceContext const* context);

private:
    /// A single line message that sums up the issue
    virtual void header(std::ostream& os, SourceContext const* ctx) const = 0;

    void formatImpl(TreeFormatter& fmt, SourceContext const* ctx) const;

    Kind _kind;
    SourceRange _sourceRange;
    SourceContext const* _sourceContext;
    std::vector<std::unique_ptr<Issue>> _children;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUE_H
