#ifndef PRISM_SEMA_SYMBOLFWD_H
#define PRISM_SEMA_SYMBOLFWD_H

#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/Rtti.h>

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

#define SEMA_SYMBOL(Name, ...) class Name;
#include <Prism/Sema/Symbol.def>

enum class SymbolType {
#define SEMA_SYMBOL(Name, ...) Name,
#include <Prism/Sema/Symbol.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(SymbolType)

namespace detail {

using NoParent = void;

}

} // namespace prism

#define SEMA_SYMBOL(Name, Parent, Corpo, ...)                                  \
    PRISM_DEFINE_RTTI(prism::Name, prism::SymbolType::Name, prism::Parent,     \
                      Corpo)
#include <Prism/Sema/Symbol.def>

namespace prism {

/// List of all builtins
enum class BuiltinSymbol {
#define SEMA_BUILTIN_TYPE(Name, ...) Name,
#include <Prism/Sema/Builtins.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(BuiltinSymbol)

} // namespace prism

#endif // PRISM_SEMA_SYMBOLFWD_H
