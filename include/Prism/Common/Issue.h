#ifndef PRISM_COMMON_ISSUE_H
#define PRISM_COMMON_ISSUE_H

#include <cstdint>
#include <functional>
#include <iosfwd>
#include <vector>

namespace prism {

class SourceContext;

/// A message to communicate a problem in the source code.
/// An issue can contain multiple messages, that describe the problem in detail
class SourceMessage {
public:
    SourceMessage(uint32_t sourceIndex = 0,
                  std::function<void(std::ostream&)> fmt = {}):
        sourceIdx(sourceIndex), fmt(std::move(fmt)) {}

    /// \Returns the location in the source code to which this message refers
    uint32_t sourceIndex() const { return sourceIdx; }

private:
    friend struct FmtImpl;

    uint32_t sourceIdx;
    std::function<void(std::ostream&)> fmt;
};

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

    /// Adds a message
    void message(std::function<void(std::ostream&)> fmt);

    /// \overload
    void message(uint32_t sourceIndex, std::function<void(std::ostream&)> fmt);

private:
    Kind _kind;
    uint32_t index;
    SourceMessage msg;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUE_H
