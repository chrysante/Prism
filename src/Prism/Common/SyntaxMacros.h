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

#define PRISM_FN_IMPL(Name, ...)                                               \
    <typename... Args>(Args && ... args)->decltype(auto) {                     \
        return Name(__VA_ARGS__ __VA_OPT__(, )((Args&&)args)...);              \
    }

#define FN(Name, ...) [this] PRISM_FN_IMPL(Name __VA_OPT__(, ) __VA_ARGS__)
#define VALFN(Name, ...)                                                       \
    [ =, this ] PRISM_FN_IMPL(Name __VA_OPT__(, ) __VA_ARGS__)
#define REFFN(Name, ...)                                                       \
    [&, this ] PRISM_FN_IMPL(Name __VA_OPT__(, ) __VA_ARGS__)

#define VALFN1(__VA_ARGS__)                                                    \
    [=](auto&& _1) -> decltype(auto) { return __VA_ARGS__; }

#endif // PRISM_COMMON_SYNTAXMACROS_H
