#ifndef PRISM_COMMON_TYPENAME_H
#define PRISM_COMMON_TYPENAME_H

#include <concepts>
#include <string>
#include <typeinfo>

namespace prism {

namespace detail {
std::string demangleName(char const* mangled);
}

template <typename T>
    requires std::is_polymorphic_v<T>
std::string getDemangledName(T const& base) {
    return detail::demangleName(typeid(base).name());
}

template <typename T>
    requires std::is_polymorphic_v<T>
std::string getDemangledName() {
    return detail::demangleName(typeid(T).name());
}

} // namespace prism

#endif // PRISM_COMMON_TYPENAME_H
