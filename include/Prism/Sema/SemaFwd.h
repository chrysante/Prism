#ifndef PRISM_SEMA_SEMAFWD_H
#define PRISM_SEMA_SEMAFWD_H

#include <cstdint>

#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/Rtti.h>
#include <Prism/Sema/SymbolFwd.inl>

namespace prism {

class SourceFileFacet;
class SourceContext;
class Obligation;
class InterfaceLike;
class Scope;

enum class ValueCat { LValue, RValue };

static constexpr ValueCat LValue = ValueCat::LValue;
static constexpr ValueCat RValue = ValueCat::RValue;

PRISM_DEFINE_ENUM_FUNCTIONS(ValueCat)

enum class Mutability { Mut, Const };

PRISM_DEFINE_ENUM_FUNCTIONS(Mutability)

enum class BindMode { Static, Dyn };

PRISM_DEFINE_ENUM_FUNCTIONS(BindMode)

enum class PassingConvention { In, Inout, Sink };

PRISM_DEFINE_ENUM_FUNCTIONS(PassingConvention)

enum class ArithmeticOperation { Add, Sub, Div, Mul, Rem };

PRISM_DEFINE_ENUM_FUNCTIONS(ArithmeticOperation)

/// List of all builtins
enum class BuiltinSymbol {
#define SEMA_BUILTIN(Name, ...) Name,
#include <Prism/Sema/Builtins.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(BuiltinSymbol)

bool isBuiltinSymbol(Symbol const& symbol);

/// \Returns the source context in which this symbol is defined or null if it's
/// compiler defined
SourceContext const* getSourceContext(Symbol const* sym);

} // namespace prism

#endif // PRISM_SEMA_SEMAFWD_H
