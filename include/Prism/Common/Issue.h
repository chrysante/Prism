#ifndef PRISM_COMMON_ISSUE_H
#define PRISM_COMMON_ISSUE_H

#include <cstdint>
#include <functional>
#include <iosfwd>
#include <vector>

namespace prism {

class SourceContext;

/// Different source message kinds
enum class MessageKind { Primary, Secondary };

/// A message to communicate a problem in the source code.
/// An issue can contain multiple messages, that describe the problem in detail
class SourceMessage {
public:
    explicit SourceMessage(MessageKind kind, uint32_t sourceIndex,
                           std::function<void(std::ostream&)> fmt):
        _kind(kind), sourceIdx(sourceIndex), fmt(std::move(fmt)) {}

    /// \Returns the kind of this message
    MessageKind kind() const { return _kind; }

    /// \Returns the location in the source code to which this message refers
    uint32_t sourceIndex() const { return sourceIdx; }

private:
    friend struct FmtImpl;

    MessageKind _kind;
    uint32_t sourceIdx;
    std::function<void(std::ostream&)> fmt;
};

/// Base class of all issues
class Issue {
public:
    virtual ~Issue() = default;

    /// Formats this issue to \p os
    void format(std::ostream& os, SourceContext const& ctx) const;

    /// \Returns the position in the source code where this issue occurred
    uint32_t sourceIndex() const { return index; }

protected:
    explicit Issue(uint32_t index): index(index) {}

    /// Adds a message
    void message(MessageKind kind, std::function<void(std::ostream&)> fmt);

    /// \overload
    void message(MessageKind kind, uint32_t sourceIndex,
                 std::function<void(std::ostream&)> fmt);

private:
    uint32_t index;
    std::vector<SourceMessage> messages;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUE_H
