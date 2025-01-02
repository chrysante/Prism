#ifndef PRISM_COMMON_ASSERT_H
#define PRISM_COMMON_ASSERT_H

#include <cassert>

#define PRISM_ASSERT(condition, ...)                                           \
    PRISM_ASSERT_IMPL(condition, __VA_ARGS__ __VA_OPT__(, ) 2, 1)
#define PRISM_ASSERT_IMPL(condition, message, count, ...)                      \
    PRISM_ASSERT_IMPL_##count(condition, message)
#define PRISM_ASSERT_IMPL_1(condition, message) assert(condition)
#define PRISM_ASSERT_IMPL_2(condition, message) assert(condition)

#define PRISM_EXPECT(condition, ...)                                           \
    PRISM_ASSERT(condition, __VA_OPT__(, ) __VA_ARGS__)

#define PRISM_ASSERT_AUDIT(condition, ...)                                     \
    PRISM_ASSERT(condition __VA_OPT__(, ) __VA_ARGS__)

#define PRISM_UNREACHABLE()                                                    \
    PRISM_ASSERT(false, "Executed unreachable code path")

#define PRISM_UNIMPLEMENTED()                                                  \
    PRISM_ASSERT(false, "Executed unimplemented code path")

#endif // PRISM_COMMON_ASSERT_H
