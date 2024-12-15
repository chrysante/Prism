#ifndef PRISM_AST_ASTFWD_H
#define PRISM_AST_ASTFWD_H

#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/NoParent.h>
#include <Prism/Common/Rtti.h>

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
    PRISM_DEFINE_RTTI(prism::Type, prism::AstNodeType::Type, prism::Parent,    \
                      Corporeality)
#include <Prism/Ast/Ast.def>

namespace prism {

/// Runtime type identifiers for the facet nodes
enum class FacetType {
#define FACET_DEF(Type, ...) Type,
#include <Prism/Ast/Facet.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(FacetType)

/// Forward declarations of all facet nodes
#define FACET_DEF(Type, ...) class Type;
#include <Prism/Ast/Facet.def>

} // namespace prism

// Register the facet nodes with CSP
#define FACET_DEF(Type, Parent, Corporeality)                                  \
    PRISM_DEFINE_RTTI(prism::Type, prism::FacetType::Type, prism::Parent,      \
                      Corporeality)
#include <Prism/Ast/Facet.def>

#endif // PRISM_AST_ASTFWD_H
