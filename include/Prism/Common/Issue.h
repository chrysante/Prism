#ifndef PRISM_COMMON_ISSUE_H
#define PRISM_COMMON_ISSUE_H

#include <cstdint>
#include <functional>
#include <iosfwd>
#include <vector>

namespace prism {

class SourceContext;

///
enum class MessageKind { Primary, Secondary };

///
class SourceMessage {
public:
    explicit SourceMessage(MessageKind kind, uint32_t sourceIndex,
                           std::function<void(std::ostream&)> fmt):
        _kind(kind), sourceIdx(sourceIndex), fmt(std::move(fmt)) {}

    MessageKind kind() const { return _kind; }

    uint32_t sourceIndex() const { return sourceIdx; }

private:
    friend struct FmtImpl;

    MessageKind _kind;
    uint32_t sourceIdx;
    std::function<void(std::ostream&)> fmt;
};

///
class Issue {
public:
    virtual ~Issue() = default;

    void format(std::ostream& str, SourceContext const& ctx) const;

protected:
    explicit Issue(uint32_t index): index(index) {}

    void message(MessageKind kind, std::function<void(std::ostream&)> fmt);
    void message(MessageKind kind, uint32_t sourceIndex,
                 std::function<void(std::ostream&)> fmt);

private:
    uint32_t index;
    std::vector<SourceMessage> messages;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUE_H
