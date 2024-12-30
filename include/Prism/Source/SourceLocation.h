#ifndef PRISM_SOURCE_SOURCELOCATION_H
#define PRISM_SOURCE_SOURCELOCATION_H

#include <cstdint>

namespace prism {

class SourceContext;

///
struct SourceLocation {
    uint32_t index = 0;
    uint32_t line = 0;
    uint32_t column = 0;

    bool operator==(SourceLocation const&) const = default;
};

///
struct SourceRange {
    uint32_t index = 0, length = 0;

    bool operator==(SourceRange const&) const = default;
};

///
struct FullSourceRange {
    SourceContext const* context = nullptr;
    SourceLocation begin{}, end{};

    /// \Returns the corresponding "slim" source range
    SourceRange slim() const {
        return { begin.index, end.index - begin.index };
    }
};

} // namespace prism

#endif // PRISM_SOURCE_SOURCELOCATION_H
