#ifndef PRISM_SEMA2_SEMAFWD_H
#define PRISM_SEMA2_SEMAFWD_H

#include <cstdint>

#include <APMath/APInt.h>

#include <Prism/Common/EnumUtil.h>
#include <Prism/Sema2/SymbolFwd.inl>

namespace prism {

class Scope;
class SemaContext;

class GenValueParam;

using APMath::APInt;

///
enum class PassingConvention : uint8_t { In, Inout, Sink };

PRISM_DEFINE_ENUM_FUNCTIONS(PassingConvention)

///
enum class Mutability : uint8_t { Mut, Const };

PRISM_DEFINE_ENUM_FUNCTIONS(Mutability)

///
enum class ValueCat : uint8_t { LValue, RValue };

PRISM_DEFINE_ENUM_FUNCTIONS(ValueCat)

} // namespace prism

#endif // PRISM_SEMA2_SEMAFWD_H
