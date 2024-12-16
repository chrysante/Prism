#ifndef PRISM_COMMON_MACROUTILS_H
#define PRISM_COMMON_MACROUTILS_H

#define PRISM_COUNT_ARGS_IMPL(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, N, ...) N
#define PRISM_COUNT_ARGS(...)                                                  \
    PRISM_COUNT_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(, ) 10, 9, 8, 7, 6, 5, 4, 3,  \
                          2, 1, 0)

#define PRISM_CONCAT(x, y)      PRISM_CONCAT_IMPL(x, y)
#define PRISM_CONCAT_IMPL(x, y) x##y

#define PRISM_REMOVE_PARENS(...)      PRISM_REMOVE_PARENS_IMPL(__VA_ARGS__)
#define PRISM_REMOVE_PARENS_IMPL(...) __VA_ARGS__

#define PRISM_FOR_EACH(macro, Sep, ...)                                        \
    PRISM_CONCAT(PRISM_FOR_EACH_IMPL_,                                         \
                 PRISM_COUNT_ARGS(__VA_ARGS__))(macro, Sep __VA_OPT__(, )      \
                                                           __VA_ARGS__)

#define PRISM_COMMA() ,
#define PRISM_NONE()

#define PRISM_FOR_EACH_IMPL_0(macro, Sep)
#define PRISM_FOR_EACH_IMPL_1(macro, Sep, a)    macro(a)
#define PRISM_FOR_EACH_IMPL_2(macro, Sep, a, b) macro(a) Sep() macro(b)
#define PRISM_FOR_EACH_IMPL_3(macro, Sep, a, b, c)                             \
    macro(a) Sep() macro(b) Sep() macro(c)
#define PRISM_FOR_EACH_IMPL_4(macro, Sep, a, b, c, d)                          \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d)
#define PRISM_FOR_EACH_IMPL_5(macro, Sep, a, b, c, d, e)                       \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d) Sep() macro(e)
#define PRISM_FOR_EACH_IMPL_6(macro, Sep, a, b, c, d, e, f)                    \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d) Sep() macro(e) Sep() \
        macro(f)
#define PRISM_FOR_EACH_IMPL_7(macro, Sep, a, b, c, d, e, f, g)                 \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d) Sep() macro(e) Sep() \
        macro(f) Sep() macro(g)
#define PRISM_FOR_EACH_IMPL_8(macro, Sep, a, b, c, d, e, f, g, h)              \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d) Sep() macro(e) Sep() \
        macro(f) Sep() macro(g) Sep() macro(h)
#define PRISM_FOR_EACH_IMPL_9(macro, Sep, a, b, c, d, e, f, g, h, i)           \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d) Sep() macro(e) Sep() \
        macro(f) Sep() macro(g) Sep() macro(h) Sep() macro(i)
#define PRISM_FOR_EACH_IMPL_10(macro, Sep, a, b, c, d, e, f, g, h, i, j)       \
    macro(a) Sep() macro(b) Sep() macro(c) Sep() macro(d) Sep() macro(e) Sep() \
        macro(f) Sep() macro(g) Sep() macro(h) Sep() macro(i) Sep() macro(j)

#endif // PRISM_COMMON_MACROUTILS_H
