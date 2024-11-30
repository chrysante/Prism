#ifndef PRISM_AST_ASTFWD_H
#define PRISM_AST_ASTFWD_H

#include <csp.hpp>

#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/NoParent.h>

namespace prism {

/// Runtime type identifiers for the AST nodes
enum class AstNodeType {
#define AST_NODE(Type, ...) Type,
#include <Prism/Ast/Ast.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(AstNodeType)

/// Forward declarations of all AST nodes
#define AST_NODE(Type, ...) class Type;
#include <Prism/Ast/Ast.def>

} // namespace prism

// Register the AST nodes with CSP
#define AST_NODE(Type, Parent, Corporeality)                                   \
    CSP_DEFINE(prism::Type, prism::AstNodeType::Type, prism::Parent,           \
               Corporeality)
#include <Prism/Ast/Ast.def>

#endif // PRISM_AST_ASTFWD_H
