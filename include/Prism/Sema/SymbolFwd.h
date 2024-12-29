#ifndef PRISM_SEMA_SYMBOLFWD_H
#define PRISM_SEMA_SYMBOLFWD_H

#include <cstdint>

#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/Rtti.h>
#include <Prism/Sema/SymbolFwd.inl>

namespace prism {

enum class ValueCat { LValue, RValue };

static constexpr ValueCat LValue = ValueCat::LValue;
static constexpr ValueCat RValue = ValueCat::RValue;

PRISM_DEFINE_ENUM_FUNCTIONS(ValueCat)

enum class Mutability { Mut, Const };

PRISM_DEFINE_ENUM_FUNCTIONS(Mutability)

enum class BindMode { Static, Dyn };

PRISM_DEFINE_ENUM_FUNCTIONS(BindMode)

enum class ArithmeticOperation { Add, Sub, Div, Mul, Rem };

PRISM_DEFINE_ENUM_FUNCTIONS(ArithmeticOperation)

/// List of all builtins
enum class BuiltinSymbol {
#define SEMA_BUILTIN_TYPE(Name, ...) Name,
#include <Prism/Sema/Builtins.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(BuiltinSymbol)

bool isBuiltinSymbol(Symbol const& symbol);

/// Memory layout of a type, i.e., size, stride and alignment in bytes
class TypeLayout {
public:
    /// Incomplete layout means the type and size is not known
    static TypeLayout const Incomplete;

    /// Constructs a complete layout
    TypeLayout(size_t size, size_t stride, size_t align):
        _size(size), _stride(stride), _align(align) {
        PRISM_ASSERT(align < (size_t)-1, "-1 is reserved for incomplete types");
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

    /// \Returns true if this size is complete
    bool isComplete() const { return _align != (size_t)-1; }

    /// \Returns `isComplete()`
    explicit operator bool() const { return isComplete(); }

    ///
    bool operator==(TypeLayout const&) const = default;

private:
    TypeLayout() = default;

    size_t _size = 0;
    size_t _stride = 0;
    size_t _align = (size_t)-1;
};

inline constexpr TypeLayout TypeLayout::Incomplete{};

std::ostream& operator<<(std::ostream& ostream, TypeLayout layout);

} // namespace prism

#endif // PRISM_SEMA_SYMBOLFWD_H
