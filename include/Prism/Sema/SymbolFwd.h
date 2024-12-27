#ifndef PRISM_SEMA_SYMBOLFWD_H
#define PRISM_SEMA_SYMBOLFWD_H

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

} // namespace prism

#endif // PRISM_SEMA_SYMBOLFWD_H
