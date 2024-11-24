#ifndef PRISM_SOURCE_SOURCELOCATION_H
#define PRISM_SOURCE_SOURCELOCATION_H

#include <cstdint>

namespace prism {

///
struct SourceLocation {
    uint32_t index;
    uint32_t line;
    uint32_t column;
};

} // namespace prism

#endif // PRISM_SOURCE_SOURCELOCATION_H
