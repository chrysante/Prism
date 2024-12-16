#ifndef PRISM_AST_FACET_H
#define PRISM_AST_FACET_H

#include <algorithm>
#include <bit>
#include <iosfwd>
#include <span>

#include <utl/ipp.hpp>

#include <Prism/Ast/AstFwd.h>
#include <Prism/Common/Allocator.h>
#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/NoParent.h>
#include <Prism/Common/Rtti.h>
#include <Prism/Source/Token.h>

#include <Prism/Common/MacroUtils.h>

namespace prism {

class Facet {
public:
    std::span<Facet const* const> children() const {
        return { getChildrenPtr(), getNumChildren() };
    }

    Facet const* childAt(size_t index) const {
        PRISM_ASSERT(index < getNumChildren(), "Index out of bounds");
        return getChildrenPtr()[index];
    }

    template <std::derived_from<Facet> F>
    F const* childAt(size_t index) const {
        return cast<F const*>(childAt(index));
    }

protected:
    explicit Facet(Token tok): term{ .tok = tok } {}

    explicit Facet(FacetType nodeType, std::span<Facet const* const> children):
        nonTerm{ .flag = 1,
                 .numChildren = static_cast<uint32_t>(children.size()),
                 .type = nodeType } {
        std::copy(children.begin(), children.end(), getChildrenPtr());
    }

    explicit Facet(AstNode* ast): astNode(ast, 2) {}

    friend FacetType get_rtti(Facet const& node) { return node.getNodeType(); }

    FacetType getNodeType() const {
        auto cat = getCategory();
        return cat == FacetType::NonTerminalFacet ? nonTerm.type : cat;
    }

    FacetType getCategory() const {
        using enum FacetType;
        size_t index = std::bit_cast<uint64_t>(*this) & 3;
        PRISM_ASSERT(index < 3);
        return std::array{ TerminalFacet, NonTerminalFacet,
                           AstWrapperFacet }[index];
    }

    Facet const** getChildrenPtr() const {
        return (Facet const**)((unsigned char*)this + sizeof *this);
    }

    size_t getNumChildren() const {
        return getCategory() == FacetType::NonTerminalFacet ?
                   nonTerm.numChildren :
                   0;
    }

    union alignas(void*) {
        struct {
            Token tok;
        } term;
        struct {
            uint32_t flag        : 2;
            uint32_t numChildren : 30;
            FacetType type;
        } nonTerm;
        utl::ipp<AstNode*, uint32_t, 2> astNode;
    };
};

static_assert(sizeof(Facet) == sizeof(void*));
static_assert(alignof(Facet) == alignof(void*));

template <std::derived_from<Facet> T, typename... Args>
    requires UniformConstructibleFrom<T, Args...>
T* allocate(MonotonicBufferResource& alloc, Args&&... args) {
    void* buf = alloc.allocate(sizeof(Facet) + sizeof...(Args) * sizeof(void*),
                               alignof(Facet));
    return new (buf) T(std::forward<Args>(args)...);
}

template <std::derived_from<Facet> T,
          std::convertible_to<std::span<Facet const* const>> Arg>
    requires UniformConstructibleFrom<T, Arg>
T* allocate(MonotonicBufferResource& alloc, Arg&& args) {
    void* buf = alloc.allocate(sizeof(Facet) +
                                   std::span<Facet const* const>(args).size() *
                                       sizeof(void*),
                               alignof(Facet));
    return new (buf) T(std::forward<Arg>(args));
}

class TerminalFacet: public Facet {
public:
    TerminalFacet(Token tok): Facet(tok) {}

    std::span<Facet const* const> children() const { return {}; }

    Token token() const { return term.tok; }

    friend FacetType get_rtti(TerminalFacet const& node) {
        return FacetType::TerminalFacet;
    }
};

class AstWrapperFacet: public Facet {
public:
    explicit AstWrapperFacet(AstNode* ast): Facet(ast) {}

    std::span<Facet const* const> children() const { return {}; }

    AstNode* get() const { return astNode.pointer(); }
};

class NonTerminalFacet: public Facet {
protected:
    NonTerminalFacet(FacetType type, std::span<Facet const* const> children):
        Facet(type, children) {}

    friend FacetType get_rtti(NonTerminalFacet const& node) {
        return node.nonTerm.type;
    }
};

namespace detail {

template <typename T>
struct FacetFieldTransform {
    using InterfaceType = T const*;

    static T const* toStoredType(MonotonicBufferResource*, T const* t) {
        return t;
    }

    static T const* toInterfaceType(T const* t) { return t; }
};

template <>
struct FacetFieldTransform<TerminalFacet> {
    using InterfaceType = Token;

    static TerminalFacet const* toStoredType(MonotonicBufferResource* resource,
                                             Token token) {
        return allocate<TerminalFacet>(*resource, token);
    }

    static Token toInterfaceType(TerminalFacet const* facet) {
        return facet->token();
    }
};

template <>
struct FacetFieldTransform<AstWrapperFacet> {
    using InterfaceType = AstNode*;

    static AstWrapperFacet const* toStoredType(
        MonotonicBufferResource* resource, AstNode* ast) {
        return allocate<AstWrapperFacet>(*resource, ast);
    }

    static AstNode* toInterfaceType(AstWrapperFacet const* facet) {
        return facet->get();
    }
};

template <typename T>
using FacetInterfaceType = typename FacetFieldTransform<T>::InterfaceType;

} // namespace detail

#define FACET_FIELD_CTOR_ARG_IMPL(Index, Type, Name)                           \
    , detail::FacetInterfaceType<Type> Name
#define FACET_FIELD_CTOR_ARG(...) FACET_FIELD_CTOR_ARG_IMPL __VA_ARGS__

#define FACET_FIELD_CTOR_ARG_USE_IMPL(Index, Type, Name)                       \
    detail::FacetFieldTransform<Type>::toStoredType(resource, Name)
#define FACET_FIELD_CTOR_ARG_USE(...) FACET_FIELD_CTOR_ARG_USE_IMPL __VA_ARGS__

#define FACET_FIELD_ACC_IMPL(Index, Type, Name)                                \
    detail::FacetInterfaceType<Type> Name() const {                            \
        return detail::FacetFieldTransform<Type>::toInterfaceType(             \
            childAt<Type>(Index));                                             \
    }
#define FACET_FIELD_ACC(...) FACET_FIELD_ACC_IMPL __VA_ARGS__

#define NT_FACET_DEF(Name, Parent, Corporeality, Fields)                       \
    class Name: public Parent {                                                \
    public:                                                                    \
        explicit Name(MonotonicBufferResource* resource PRISM_FOR_EACH(        \
            FACET_FIELD_CTOR_ARG, PRISM_NONE, PRISM_REMOVE_PARENS Fields)):    \
            Parent(FacetType::Name,                                            \
                   std::initializer_list<Facet const*>{                        \
                       PRISM_FOR_EACH(FACET_FIELD_CTOR_ARG_USE, PRISM_COMMA,   \
                                      PRISM_REMOVE_PARENS Fields) }) {}        \
                                                                               \
        explicit Name(std::span<Facet const* const> fields):                   \
            Parent(FacetType::Name, fields) {}                                 \
                                                                               \
        PRISM_FOR_EACH(FACET_FIELD_ACC, PRISM_NONE,                            \
                       PRISM_REMOVE_PARENS Fields)                             \
    };
#include "Prism/Ast/Facet.def"

#undef FACET_FIELD_CTOR_ARG_IMPL
#undef FACET_FIELD_CTOR_ARG
#undef FACET_FIELD_CTOR_ARG_USE_IMPL
#undef FACET_FIELD_CTOR_ARG_USE
#undef FACET_FIELD_ACC_IMPL
#undef FACET_FIELD_ACC

class SourceContext;
class TreeFormatter;

/// Prints \p facet  as a tree to \p ostream
void print(Facet const* facet, std::ostream& ostream,
           SourceContext const* srcCtx = nullptr);

/// \overload
void print(Facet const* facet, TreeFormatter& fmt,
           SourceContext const* srcCtx = nullptr);

} // namespace prism

#include <Prism/Common/MacroUtilsUndef.h>

#endif // PRISM_AST_FACET_H
