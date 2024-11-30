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

enum class AstLogicalOp : uint8_t {
#define AST_LOGICAL_OPERATOR(Name, ...) Name,
#include <Prism/Ast/Ast.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(AstLogicalOp)

enum class AstUnaryOp : uint8_t {
#define AST_UNARY_OPERATOR(Name, ...) Name,
#include <Prism/Ast/Ast.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(AstUnaryOp)

} // namespace prism

#endif // PRISM_AST_OPERATORS_H
