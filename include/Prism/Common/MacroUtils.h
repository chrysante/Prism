#define PRISM_COUNT_ARGS_IMPL(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, N, ...) N
#define PRISM_COUNT_ARGS(...)                                                  \
    PRISM_COUNT_ARGS_IMPL(__VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

#define PRISM_CONCAT(x, y)      PRISM_CONCAT_IMPL(x, y)
#define PRISM_CONCAT_IMPL(x, y) x##y

#define PRISM_REMOVE_PARENS(...)      PRISM_REMOVE_PARENS_IMPL(__VA_ARGS__)
#define PRISM_REMOVE_PARENS_IMPL(...) __VA_ARGS__

#define PRISM_FOR_EACH(macro, ...)                                             \
    PRISM_CONCAT(PRISM_FOR_EACH_IMPL_,                                         \
                 PRISM_COUNT_ARGS(__VA_ARGS__))(macro, __VA_ARGS__)

#define PRISM_SEPARATOR ,

#define PRISM_FOR_EACH_IMPL_1(macro, a)    macro(a)
#define PRISM_FOR_EACH_IMPL_2(macro, a, b) macro(a) PRISM_SEPARATOR macro(b)
#define PRISM_FOR_EACH_IMPL_3(macro, a, b, c)                                  \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)
#define PRISM_FOR_EACH_IMPL_4(macro, a, b, c, d)                               \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)
#define PRISM_FOR_EACH_IMPL_5(macro, a, b, c, d, e)                            \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)                                                   \
    PRISM_SEPARATOR macro(e)
#define PRISM_FOR_EACH_IMPL_6(macro, a, b, c, d, e, f)                         \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)                                                   \
    PRISM_SEPARATOR macro(e)                                                   \
    PRISM_SEPARATOR macro(f)
#define PRISM_FOR_EACH_IMPL_7(macro, a, b, c, d, e, f, g)                      \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)                                                   \
    PRISM_SEPARATOR macro(e)                                                   \
    PRISM_SEPARATOR macro(f)                                                   \
    PRISM_SEPARATOR macro(g)
#define PRISM_FOR_EACH_IMPL_8(macro, a, b, c, d, e, f, g, h)                   \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)                                                   \
    PRISM_SEPARATOR macro(e)                                                   \
    PRISM_SEPARATOR macro(f)                                                   \
    PRISM_SEPARATOR macro(g)                                                   \
    PRISM_SEPARATOR macro(h)
#define PRISM_FOR_EACH_IMPL_9(macro, a, b, c, d, e, f, g, h, i)                \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)                                                   \
    PRISM_SEPARATOR macro(e)                                                   \
    PRISM_SEPARATOR macro(f)                                                   \
    PRISM_SEPARATOR macro(g)                                                   \
    PRISM_SEPARATOR macro(h)                                                   \
    PRISM_SEPARATOR macro(i)
#define PRISM_FOR_EACH_IMPL_10(macro, a, b, c, d, e, f, g, h, i, j)            \
    macro(a) PRISM_SEPARATOR macro(b)                                          \
    PRISM_SEPARATOR macro(c)                                                   \
    PRISM_SEPARATOR macro(d)                                                   \
    PRISM_SEPARATOR macro(e)                                                   \
    PRISM_SEPARATOR macro(f)                                                   \
    PRISM_SEPARATOR macro(g)                                                   \
    PRISM_SEPARATOR macro(h)                                                   \
    PRISM_SEPARATOR macro(i)                                                   \
    PRISM_SEPARATOR macro(j)
