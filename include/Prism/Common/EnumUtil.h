#ifndef PRISM_COMMON_ENUMUTIL_H
#define PRISM_COMMON_ENUMUTIL_H

#include <iosfwd>
#include <string>
#include <string_view>

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

#endif // PRISM_COMMON_ENUMUTIL_H
