#ifndef PRISM_SEMA_TYPELAYOUT_H
#define PRISM_SEMA_TYPELAYOUT_H

#include <iosfwd>

#include <Prism/Common/Assert.h>

namespace prism {

/// Memory layout of a type, i.e., size, stride and alignment in bytes
class TypeLayout {
public:
    /// Incomplete layout means the memory layout is unknown
    static TypeLayout const Incomplete;

    /// Poison layout means the type and size is computable because of
    /// definition errors
    static TypeLayout const Poison;

    /// Constructs a complete layout
    TypeLayout(size_t size, size_t stride, size_t align):
        _size(size), _stride(stride), _align(align) {
        PRISM_ASSERT(align < (size_t)-2,
                     "-1 and -2 are reserved for incomplete/poison layouts");
    }

    /// Convenience constructor for layouts where all values are equal
    explicit TypeLayout(size_t sizeStrideAlign):
        TypeLayout(sizeStrideAlign, sizeStrideAlign, sizeStrideAlign) {}

    /// \Returns the size.
    /// \pre Must be complete
    size_t size() const {
        PRISM_ASSERT(isComplete());
        return _size;
    }

    /// \Returns the stride.
    /// \pre Must be complete
    size_t stride() const {
        PRISM_ASSERT(isComplete());
        return _stride;
    }

    /// \Returns the alignment.
    /// \pre Must be complete
    size_t alignment() const {
        PRISM_ASSERT(isComplete());
        return _align;
    }

    /// \Returns true if this layout is complete
    bool isComplete() const { return _align < (size_t)-2; }

    /// \Returns true if this layout is incomplete
    bool isIncomplete() const { return *this == TypeLayout::Incomplete; }

    /// \Returns true if this layout is poison
    bool isPoison() const { return *this == TypeLayout::Poison; }

    /// \Returns `isComplete()`
    explicit operator bool() const { return isComplete(); }

    ///
    bool operator==(TypeLayout const&) const = default;

private:
    enum class PrivTag : int;
    explicit constexpr TypeLayout(PrivTag, size_t alignVal): _align(alignVal) {}

    size_t _size = 0;
    size_t _stride = 0;
    size_t _align = (size_t)-1;
};

inline constexpr TypeLayout TypeLayout::Incomplete(PrivTag{}, (size_t)-1);
inline constexpr TypeLayout TypeLayout::Poison(PrivTag{}, (size_t)-2);

std::ostream& operator<<(std::ostream& ostream, TypeLayout layout);

} // namespace prism

#endif // PRISM_SEMA_TYPELAYOUT_H
