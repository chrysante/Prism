#ifndef PRISM_COMMON_ALLOCATOR_H
#define PRISM_COMMON_ALLOCATOR_H

#include <concepts>
#include <memory>
#include <span>

#include <Prism/Common/Assert.h>

namespace prism {

/// Generic allocator concept
template <typename R>
concept MemoryResource = requires(R& r) {
    {
        r.allocate(size_t{}, size_t{})
    } -> std::convertible_to<void*>;
    {
        r.deallocate((void*)nullptr, size_t{}, size_t{})
    };
};

namespace detail {

template <typename T, typename Allocator, typename... Args>
concept AllocConstructibleBullet1 =
    std::constructible_from<T, std::allocator_arg_t, Allocator, Args...>;

template <typename T, typename Allocator, typename... Args>
concept AllocConstructibleBullet2 =
    std::constructible_from<T, Args..., Allocator>;

template <typename T, typename Allocator, typename... Args>
concept AllocConstructibleBullet3 = std::constructible_from<T, Args...>;

template <typename T, typename Allocator, typename... Args>
concept AllocConstructible = AllocConstructibleBullet1<T, Allocator, Args...> ||
                             AllocConstructibleBullet2<T, Allocator, Args...> ||
                             AllocConstructibleBullet3<T, Allocator, Args...>;

} // namespace detail

///
template <typename T, typename R>
class ResourceAllocator {
public:
    using value_type = T;

    ResourceAllocator(ResourceAllocator const&) = default;

    template <typename S>
    ResourceAllocator(ResourceAllocator<S, R> const& other) noexcept:
        r(other.resource()) {}

    ResourceAllocator(R* resource): r(resource) {}

    ResourceAllocator& operator=(ResourceAllocator const&) = delete;

    T* allocate(size_t n) {
        return static_cast<T*>(resource()->allocate(n * sizeof(T), alignof(T)));
    }

    void deallocate(T* p, size_t n) {
        PRISM_ASSERT(p != nullptr, "passing NULL is not allowed");
        resource()->deallocate(p, n * sizeof(T), alignof(T));
    }

    template <typename U, typename... Args>
        requires detail::AllocConstructible<T, R*, Args...>
    void construct(U* p, Args&&... args) {
        if constexpr (detail::AllocConstructibleBullet1<T, R*, Args...>) {
            std::construct_at(p, std::allocator_arg, resource(),
                              std::forward<Args>(args)...);
        }
        else if constexpr (detail::AllocConstructibleBullet2<T, R*, Args...>) {
            std::construct_at(p, std::forward<Args>(args)..., resource());
        }
        else {
            static_assert(detail::AllocConstructibleBullet3<T, R*, Args...>);
            std::construct_at(p, std::forward<Args>(args)...);
        }
    }

    void* allocate_bytes(size_t size,
                         size_t alignment = alignof(std::max_align_t)) {
        return resource()->allocate(size, alignment);
    }

    void deallocate_bytes(void* p, size_t size,
                          size_t alignment = alignof(std::max_align_t)) {
        resource()->deallocate(p, size, alignment);
    }

    R* resource() const noexcept { return r; }

private:
    R* r;
};

template <typename T, typename U, typename R>
bool operator==(ResourceAllocator<T, R> const& lhs,
                ResourceAllocator<U, R> const& rhs) noexcept {
    return *lhs.resource() == *rhs.resource();
}

/// "Arena" allocator. Allocation increases a pointer in the current memory
/// block or allocates a new block. New blocks grow geometrically in size.
/// Deallocation is a no-op. Memory gets freed when the allocator is destroyed
/// or when `release()` is called.
class MonotonicBufferResource {
public:
    /// Default value for the size of the first allocated block
    static constexpr size_t InititalSize = 128;

    MonotonicBufferResource();
    explicit MonotonicBufferResource(size_t initSize);
    MonotonicBufferResource(MonotonicBufferResource&&) noexcept;
    ~MonotonicBufferResource();
    MonotonicBufferResource& operator=(MonotonicBufferResource&&) noexcept;

    /// Allocate \p size number of bytes aligned to boundary specified by \p
    /// align
    void* allocate(size_t size, size_t align);

    /// Deallocate \p ptr
    /// This function is a no-op and only exists for symmetry with `allocate()`
    void deallocate(void*, size_t, size_t) {}

    /// Releases the buffer chain and dellocates all memory
    void release();

private:
    struct InternalBufferHeader {
        InternalBufferHeader* prev;
        size_t size;
    };

    void addChunk(size_t size);

    InternalBufferHeader* buffer = nullptr;
    std::byte* current = nullptr;
    std::byte* end = nullptr;
};

inline bool operator==(MonotonicBufferResource const& a,
                       MonotonicBufferResource const& b) {
    return &a == &b;
}

template <typename T, typename... Args>
concept UniformConstructibleFrom =
    requires(Args&&... args) { T{ std::forward<Args>(args)... }; };

/// Allocates memory for object of type `T` using the allocator \p alloc and
/// constructs the object with arguments \p args... \Returns a pointer the the
/// constructed object
template <typename T, typename... Args>
    requires UniformConstructibleFrom<T, Args...>
T* allocate(MonotonicBufferResource& alloc, Args&&... args) {
    T* result = static_cast<T*>(alloc.allocate(sizeof(T), alignof(T)));
    std::construct_at(result, std::forward<Args>(args)...);
    return result;
}

/// Allocates memory for an array of element type `T` with \p count elements
/// using the allocator \p alloc Does not construct the elements
template <typename T>
T* allocateArrayUninit(MonotonicBufferResource& alloc, size_t count) {
    T* result = static_cast<T*>(alloc.allocate(count * sizeof(T), alignof(T)));
    return result;
}

/// Allocates memory for an array of element type `T` with \p count elements
/// using the allocator \p alloc and default constructs the elements
template <typename T, typename Itr>
std::span<T> allocateArray(MonotonicBufferResource& alloc, Itr begin, Itr end) {
    ssize_t scount = std::distance(begin, end);
    PRISM_ASSERT(scount > 0);
    size_t count = (size_t)scount;
    T* result = allocateArrayUninit<T>(alloc, count);
    std::uninitialized_copy(begin, end, result);
    return { result, count };
}

} // namespace prism

inline void* operator new(size_t size, prism::MonotonicBufferResource& alloc) {
    return alloc.allocate(size, alignof(std::max_align_t));
}

#endif // PRISM_COMMON_ALLOCATOR_H
