#ifndef PRISM_COMMON_ISSUE_H
#define PRISM_COMMON_ISSUE_H

#include <cstdint>
#include <functional>
#include <iosfwd>
#include <vector>

namespace prism {

class SourceContext;

/// Base class of all issues
class Issue {
public:
    enum Kind { Error, Warning, Note };

    virtual ~Issue() = default;

    /// Formats this issue to \p os
    void format(std::ostream& os, SourceContext const& ctx) const;

    ///
    Kind kind() const { return _kind; }

    /// \Returns the position in the source code where this issue occurred
    uint32_t sourceIndex() const { return index; }

protected:
    explicit Issue(Kind kind, uint32_t index): _kind(kind), index(index) {}

private:
    virtual void doFormat(std::ostream& os, SourceContext const& ctx) const = 0;

    Kind _kind;
    uint32_t index;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUE_H
