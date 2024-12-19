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

#endif // PRISM_COMMON_SYNTAXMACROS_H
