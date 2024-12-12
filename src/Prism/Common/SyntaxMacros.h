#include <concepts>

#define eval internal::DoToken{}->*[&]

#define eval_as(...) eval()->__VA_ARGS__

namespace internal {

enum class DoToken : int;

template <std::invocable F>
decltype(auto) operator->*(DoToken, F&& f) {
    return ((F&&)f)();
}

} // namespace internal
