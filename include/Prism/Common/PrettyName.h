#ifndef PRISM_COMMON_PRETTYNAME_H
#define PRISM_COMMON_PRETTYNAME_H

#include <string>
#include <string_view>

namespace prism {

struct PrettyNameOptions {
    bool capitalizeFirst = false;
    bool capitalizeRest = false;
};

/// Converts the `PascalCase` or `camelCase` or `snake_case` name \p name to
/// individual words capitalized according to \p options
std::string prettyName(std::string_view name, PrettyNameOptions options = {});

/// \overload for enums
template <typename E>
    requires std::is_enum_v<E>
std::string prettyName(E e, PrettyNameOptions options = {}) {
    return prettyName(to_string_view(e), options);
}

} // namespace prism

#endif // PRISM_COMMON_PRETTYNAME_H
