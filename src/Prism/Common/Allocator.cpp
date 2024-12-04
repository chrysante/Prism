#include "Prism/Common/Allocator.h"

#include <cstdlib>

#include <utl/utility.hpp>

using namespace prism;

static std::byte* alignPointer(std::byte* ptr, size_t alignment) {
    static_assert(sizeof(size_t) == sizeof(ptr));
    size_t const r =
        utl::fast_mod_pow_two(reinterpret_cast<size_t>(ptr), alignment);
    ptr += alignment * !!r - r;
    return ptr;
}

MonotonicBufferResource::MonotonicBufferResource() {}

MonotonicBufferResource::MonotonicBufferResource(size_t initSize) {
    addChunk(initSize);
}

MonotonicBufferResource::MonotonicBufferResource(
    MonotonicBufferResource&& rhs) noexcept:
    buffer(rhs.buffer), current(rhs.current), end(rhs.end) {
    rhs.buffer = nullptr;
    rhs.current = nullptr;
    rhs.end = nullptr;
}

MonotonicBufferResource::~MonotonicBufferResource() { release(); }

MonotonicBufferResource& MonotonicBufferResource::operator=(
    MonotonicBufferResource&& rhs) noexcept {
    release();
    buffer = rhs.buffer;
    current = rhs.current;
    end = rhs.end;
    rhs.buffer = nullptr;
    rhs.current = nullptr;
    rhs.end = nullptr;
    return *this;
}

void* MonotonicBufferResource::allocate(size_t size, size_t align) {
    std::byte* result = alignPointer(current, align);
    if (end - result < size) {
        addChunk(std::max(size, buffer ? buffer->size * 2 : InititalSize));
        result = alignPointer(current, align);
    }
    current += size;
    return result;
}

void MonotonicBufferResource::release() {
    InternalBufferHeader* buf = buffer;
    while (buf) {
        size_t const size = buf->size;
        InternalBufferHeader* const prev = buf->prev;
        std::free(buf);
        (void)size;
        buf = prev;
    }
    buffer = nullptr;
    current = nullptr;
    end = nullptr;
}

void MonotonicBufferResource::addChunk(size_t size) {
    InternalBufferHeader* const newBuffer = static_cast<InternalBufferHeader*>(
        std::malloc(size + sizeof(InternalBufferHeader)));
    newBuffer->prev = buffer;
    newBuffer->size = size;

    buffer = newBuffer;
    current =
        reinterpret_cast<std::byte*>(newBuffer) + sizeof(InternalBufferHeader);
    end = current + size;
}
