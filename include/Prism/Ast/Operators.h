#ifndef PRISM_AST_OPERATORS_H
#define PRISM_AST_OPERATORS_H

#include <cstdint>

#include <Prism/Common/EnumUtil.h>

namespace prism {

enum class AstArithmeticOp : uint8_t {
#define AST_ARITHMETIC_OPERATOR(Name, ...) Name,
#include <Prism/Ast/Ast.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(AstArithmeticOp)

} // namespace prism

#endif // PRISM_AST_OPERATORS_H
