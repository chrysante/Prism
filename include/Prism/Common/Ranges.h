#ifndef PRISM_COMMON_RANGES_H
#define PRISM_COMMON_RANGES_H

#include <type_traits>

#include <range/v3/view.hpp>
#include <utl/vector.hpp>

namespace prism {

namespace detail {

template <typename T, size_t N, typename A>
struct ToSmallVectorImpl {
    template <ranges::range R>
    friend auto operator|(R&& rng, ToSmallVectorImpl) {
        using U = typename std::conditional_t<
            std::is_same_v<T, void>,
            std::remove_cvref<ranges::range_value_t<R>>,
            std::type_identity<T>>::type;
        using B =
            std::conditional_t<std::is_same_v<A, void>, std::allocator<U>, A>;
        static constexpr size_t M =
            N == size_t(-1) ? utl::default_inline_capacity<U, B> : N;
        return std::forward<R>(rng) | ranges::to<utl::small_vector<U, M, B>>;
    }
};

} // namespace detail

template <typename T = void, size_t N = size_t(-1), typename A = void>
inline constexpr detail::ToSmallVectorImpl<T, N, A> ToSmallVector{};

} // namespace prism

#endif // PRISM_COMMON_RANGES_H
