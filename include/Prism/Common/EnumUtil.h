#ifndef PRISM_COMMON_ENUMUTIL_H
#define PRISM_COMMON_ENUMUTIL_H

#include <iosfwd>
#include <string>
#include <string_view>
#include <type_traits>

#include <magic_enum/magic_enum.hpp>

#define PRISM_DEFINE_ENUM_FUNCTIONS(EnumName)                                  \
    inline std::string_view to_string_view(EnumName e) {                       \
        return magic_enum::enum_name(e);                                       \
    }                                                                          \
                                                                               \
    inline std::string to_string(EnumName e) {                                 \
        return std::string(to_string_view(e));                                 \
    }                                                                          \
                                                                               \
    template <typename CharT, typename Traits>                                 \
    std::basic_ostream<CharT, Traits>&                                         \
        operator<<(std::basic_ostream<CharT, Traits>& os, EnumName e) {        \
        return os << to_string_view(e);                                        \
    }

namespace prism {

/// The number of enumerators in \p E
template <typename E>
    requires std::is_enum_v<E>
inline constexpr size_t EnumCount = magic_enum::enum_count<E>();

} // namespace prism

#endif // PRISM_COMMON_ENUMUTIL_H
