#include "Prism/Sema/TypeLayout.h"

#include <ostream>

using namespace prism;

std::ostream& prism::operator<<(std::ostream& str, TypeLayout layout) {
    if (layout.isIncomplete()) return str << "Incomplete";
    if (layout.isPoison()) return str << "Poison";
    return str << "size: " << layout.size() << ", stride: " << layout.stride()
               << ", align: " << layout.alignment();
}
