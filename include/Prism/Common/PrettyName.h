#ifndef PRISM_COMMON_PRETTYNAME_H
#define PRISM_COMMON_PRETTYNAME_H

#include <string>
#include <string_view>

namespace prism {

struct PrettyNameOptions {
    bool capitalize_first = false;
    bool capitalize_rest = false;
};

/// Converts the `PascalCase` or `camelCase` or `snake_case` name \p name to
/// individual words capitalized according to \p options
std::string pretty_name(std::string_view name, PrettyNameOptions options = {});

/// \overload for enums
template <typename E>
    requires std::is_enum_v<E>
std::string pretty_name(E e, PrettyNameOptions options = {}) {
    return pretty_name(to_string_view(e), options);
}

} // namespace prism

#endif // PRISM_COMMON_PRETTYNAME_H
