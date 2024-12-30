#ifndef PRISM_COMMON_SYNTAXMACROS_H
#define PRISM_COMMON_SYNTAXMACROS_H

#include <concepts>

#define EVAL internal::DoToken{}->*[&]

#define EVAL_AS(...) EVAL()->__VA_ARGS__

namespace internal {

enum class DoToken : int;

template <std::invocable F>
decltype(auto) operator->*(DoToken, F&& f) {
    return ((F&&)f)();
}

} // namespace internal

#define FN(Name)                                                               \
    [this]<typename... Args>(Args&&... args) -> decltype(auto) {               \
        return Name(((Args&&)args)...);                                        \
    }

// FN0: Zero arguments
#define FN0(...) PRISM_FN0_IMPL(__VA_ARGS__, 2, 1)
#define PRISM_FN0_IMPL(Capture, Expr, NumArgs, ...)                            \
    PRISM_FN0_IMPL_##NumArgs(Capture, Expr)
#define PRISM_FN0_IMPL_1(Expr, ...) PRISM_FN0_IMPL_2(, Expr)
#define PRISM_FN0_IMPL_2(Capture, Expr)                                        \
    [Capture]() -> decltype(auto) { return Expr; }

// FN1: One argument
#define FN1(...) PRISM_FN1_IMPL(__VA_ARGS__, 2, 1)
#define PRISM_FN1_IMPL(Capture, Expr, NumArgs, ...)                            \
    PRISM_FN1_IMPL_##NumArgs(Capture, Expr)
#define PRISM_FN1_IMPL_1(Expr, ...) PRISM_FN1_IMPL_2(, Expr)
#define PRISM_FN1_IMPL_2(Capture, Expr)                                        \
    [Capture]([[maybe_unused]] auto&& _1) -> decltype(auto) { return Expr; }

// FN2: Two arguments
#define FN2(...) PRISM_FN2_IMPL(__VA_ARGS__, 2, 1)
#define PRISM_FN2_IMPL(Capture, Expr, NumArgs, ...)                            \
    PRISM_FN2_IMPL_##NumArgs(Capture, Expr)
#define PRISM_FN2_IMPL_1(Expr, ...) PRISM_FN2_IMPL_2(, Expr)
#define PRISM_FN2_IMPL_2(Capture, Expr)                                        \
    [Capture]([[maybe_unused]] auto&& _1,                                      \
              [[maybe_unused]] auto&& _2) -> decltype(auto) { return Expr; }

// FN3: Three arguments
#define FN3(...) PRISM_FN3_IMPL(__VA_ARGS__, 2, 1)
#define PRISM_FN3_IMPL(Capture, Expr, NumArgs, ...)                            \
    PRISM_FN3_IMPL_##NumArgs(Capture, Expr)
#define PRISM_FN3_IMPL_1(Expr, ...) PRISM_FN3_IMPL_2(, Expr)
#define PRISM_FN3_IMPL_2(Capture, Expr)                                        \
    [Capture]([[maybe_unused]] auto&& _1, [[maybe_unused]] auto&& _2,          \
              [[maybe_unused]] auto&& _3) -> decltype(auto) { return Expr; }

#endif // PRISM_COMMON_SYNTAXMACROS_H
