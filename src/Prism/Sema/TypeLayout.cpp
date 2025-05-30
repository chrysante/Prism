#include "Prism/Sema/TypeLayout.h"

#include <ostream>

using namespace prism;

std::ostream& prism::operator<<(std::ostream& str, TypeLayout layout) {
    if (layout.is_incomplete()) return str << "incomplete";
    if (layout.is_poison()) return str << "poison";
    return str << "size: " << layout.size() << ", stride: " << layout.stride()
               << ", align: " << layout.alignment();
}
