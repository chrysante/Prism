#include <vector>

#include <catch2/catch_test_macros.hpp>

#include "Prism/Common/Allocator.h"

using namespace prism;

TEST_CASE("Aligned allocations", "[allocator]") {
    MonotonicBufferResource alloc(128);
    for (int i = 0; i < 100; ++i) {
        std::byte* ptr = static_cast<std::byte*>(alloc.allocate(16, 8));
        CHECK(reinterpret_cast<size_t>(ptr) % 8 == 0);
        std::memset(ptr, 0, 16);
    }
}

TEST_CASE("Vector with allocator", "[allocator]") {
    MonotonicBufferResource r;
    std::vector<int, ResourceAllocator<int, MonotonicBufferResource>> v(&r);
    v.reserve(10);
}
