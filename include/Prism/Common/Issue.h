#ifndef PRISM_COMMON_ISSUE_H
#define PRISM_COMMON_ISSUE_H

#include <cstdint>
#include <iosfwd>

namespace prism {

class SourceContext;

///
class Issue {
public:
    virtual ~Issue() = default;

    void format(std::ostream& str, SourceContext const& ctx) const;

protected:
    explicit Issue(uint32_t index): index(index) {}

private:
    virtual void doFormat(std::ostream& str) const = 0;

    uint32_t index;
};

} // namespace prism

#endif // PRISM_COMMON_ISSUE_H
