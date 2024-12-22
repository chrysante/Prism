#ifndef PRISM_COMMON_ASSERT_H
#define PRISM_COMMON_ASSERT_H

#include <cassert>

#define PRISM_ASSERT(condition, ...)                                           \
    PRISM_ASSERT_IMPL(condition, ##__VA_ARGS__, 2, 1)
#define PRISM_ASSERT_IMPL(condition, message, count, ...)                      \
    PRISM_ASSERT_IMPL_##count(condition, message)
#define PRISM_ASSERT_IMPL_1(condition, message) assert(condition)
#define PRISM_ASSERT_IMPL_2(condition, message) assert(condition)

#define PRISM_UNREACHABLE() PRISM_ASSERT(false)

#define PRISM_UNIMPLEMENTED() PRISM_ASSERT(false)

#endif // PRISM_COMMON_ASSERT_H
