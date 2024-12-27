#ifndef PRISM_COMMON_FUNCTIONAL_H
#define PRISM_COMMON_FUNCTIONAL_H

#include <memory>

#define PRISM_FNOBJ_DEF(Name, TemplateArgs, Args, RetType, Expr)               \
    namespace detail {                                                         \
    struct Name##Fn {                                                          \
        TemplateArgs constexpr RetType operator() Args const                   \
            requires requires { Expr; }                                        \
        {                                                                      \
            return Expr;                                                       \
        }                                                                      \
    };                                                                         \
    }                                                                          \
    inline constexpr detail::Name##Fn Name {}

namespace prism {

PRISM_FNOBJ_DEF(Dereference, template <typename Ptr>, (Ptr && p),
                decltype(auto), *std::forward<Ptr>(p));

PRISM_FNOBJ_DEF(AddressOf, , (auto& t), auto*, std::addressof(t));

PRISM_FNOBJ_DEF(ToAddress, , (auto const& t), auto const*, std::to_address(t));

PRISM_FNOBJ_DEF(ToConstAddress, , (auto const& t), decltype(auto),
                std::to_address(t));

PRISM_FNOBJ_DEF(Get, , (auto& t), decltype(auto), t.get());

namespace detail {
template <typename T>
struct GetAsFn {
    constexpr T operator()(auto& t) const
        requires requires { t.get(); }
    {
        return t.get();
    }
};
} // namespace detail
template <typename T>
inline constexpr detail::GetAsFn<T> GetAs{};

namespace fnops {

namespace detail {

template <typename F, typename G>
struct ConcatFn {
    template <typename... Args>
    constexpr decltype(auto) operator()(Args&&... args) const
        requires std::invocable<F, std::invoke_result_t<G, Args&&...>>
    {
        return std::invoke(f, std::invoke(g, std::forward<Args>(args)...));
    }

    F f;
    G g;
};

} // namespace detail

template <typename F, typename G>
auto operator|(F&& f, G&& g) {
    return detail::ConcatFn{ std::forward<F>(f), std::forward<G>(g) };
}

} // namespace fnops

} // namespace prism

#undef PRISM_FNOBJ_DEF

#endif // PRISM_COMMON_FUNCTIONAL_H
