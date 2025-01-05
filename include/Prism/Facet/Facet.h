///
/// \File Facet.h
///
/// \Brief Facet Classes for Parse Tree Representation
///
/// This module defines the `Facet` class hierarchy, which represents the nodes
/// of a parse tree used in the parsing process of the Prism language. Each
/// `Facet` corresponds to a node in the parse tree, allowing for the
/// representation of both terminal and non-terminal constructs within the
/// language's grammar.
///
/// The `Facet` class serves as the base class, providing common functionalities
/// such as storing child nodes, accessing parent nodes, and determining the
/// type of node (terminal or non-terminal). Derived classes, such as
/// `TerminalFacet` and `NonTerminalFacet`, implement specific behaviors for
/// their respective types of nodes.
///
/// Additionally, specialized list facets (e.g., `ListFacet`, `StmtListFacet`)
/// facilitate the organization of collections of nodes, enabling the
/// representation of sequences like statements or parameters within the parse
/// tree.
///
/// Further derived facet classes are defined in the included
/// `Facets.inl` file, which extends the functionality of the base
/// `NonTerminalFacet` class and provides additional specific node types used
/// throughout the parsing process.
///

#ifndef PRISM_FACET_FACET_H
#define PRISM_FACET_FACET_H

#include <bit>
#include <iosfwd>
#include <span>

#include <range/v3/range_concepts.hpp>
#include <range/v3/size.hpp>
#include <utl/function_view.hpp>

#include <Prism/Common/Allocator.h>
#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>
#include <Prism/Facet/FacetFwd.h>
#include <Prism/Source/Token.h>

#include <Prism/Common/MacroUtils.h>

namespace prism {

/// Base class of all facets
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

    Facet const* parent() const { return _parent; }

protected:
    explicit Facet(Token tok): data{ .term{ .tok = tok } } {}

    explicit Facet(FacetType nodeType, std::span<Facet const* const> children);

    friend FacetType get_rtti(Facet const& node) { return node.getNodeType(); }

    FacetType getNodeType() const {
        auto cat = getCategory();
        return cat == FacetType::NonTerminalFacet ? data.nonTerm.type : cat;
    }

    FacetType getCategory() const {
        using enum FacetType;
        size_t index = std::bit_cast<uint64_t>(data) & 1;
        return std::array{ TerminalFacet, NonTerminalFacet }[index];
    }

    Facet const** getChildrenPtr() const {
        return (Facet const**)((unsigned char*)this + sizeof *this);
    }

    size_t getNumChildren() const {
        return getCategory() == FacetType::NonTerminalFacet ?
                   data.nonTerm.numChildren :
                   0;
    }

    struct Term {
        Token tok;
    };

    struct NonTerm {
        uint32_t flag        : 2;
        uint32_t numChildren : 30;
        FacetType type;
    };

    union alignas(void*) {
        Term term;
        NonTerm nonTerm;
    } data;
    Facet const* _parent = nullptr;
};

static_assert(sizeof(Facet) == 2 * sizeof(void*));
static_assert(alignof(Facet) == alignof(void*));

template <std::derived_from<Facet> T, typename... Args>
    requires UniformConstructibleFrom<T, Args...>
T* allocate(MonotonicBufferResource& alloc, Args&&... args) {
    void* buf = alloc.allocate(sizeof(Facet) + sizeof...(Args) * sizeof(void*),
                               alignof(Facet));
    return new (buf) T(std::forward<Args>(args)...);
}

template <std::derived_from<Facet> T, ranges::range Args>
    requires UniformConstructibleFrom<T, Args>
T* allocate(MonotonicBufferResource& alloc, Args&& args) {
    void* buf =
        alloc.allocate(sizeof(Facet) + ranges::size(args) * sizeof(void*),
                       alignof(Facet));
    return new (buf) T(std::forward<Args>(args));
}

/// Single token facets. Corresponds to a terminal in the parser grammar
class TerminalFacet: public Facet {
public:
    explicit TerminalFacet(Token tok): Facet(tok) {}

    /// Specialized to avoid a branch in `Facet::children`
    /// \Returns an empty span
    std::span<Facet const* const> children() const { return {}; }

    /// \Returns the wrapped token
    Token token() const { return data.term.tok; }

private:
    friend FacetType get_rtti(TerminalFacet const&) {
        return FacetType::TerminalFacet;
    }
};

namespace detail {

template <typename T>
T const* facetToStoredType(MonotonicBufferResource&, T const* t) {
    return t;
}

inline TerminalFacet const* facetToStoredType(MonotonicBufferResource& resource,
                                              Token token) {
    return allocate<TerminalFacet>(resource, token);
}

template <typename To, typename T>
std::span<To> unsafeSpanCast(std::span<T> s) {
    return std::bit_cast<std::span<To>>(s);
}

} // namespace detail

/// Base class of all non-terminals
class NonTerminalFacet: public Facet {
protected:
    NonTerminalFacet(FacetType type, MonotonicBufferResource& resource,
                     auto const&... children):
        Facet(type, { { detail::facetToStoredType(resource, children)... } }) {}

    NonTerminalFacet(FacetType type, std::span<Facet const* const> children):
        Facet(type, children) {}

private:
    friend FacetType get_rtti(NonTerminalFacet const& node) {
        return node.data.nonTerm.type;
    }
};

#define PRISM_DEFINE_LIST_FACET(Name, ElemType, ElemsName)                     \
    class Name: public NonTerminalFacet {                                      \
    public:                                                                    \
        explicit Name(std::span<ElemType const* const> ElemsName):             \
            NonTerminalFacet(FacetType::Name,                                  \
                             detail::unsafeSpanCast<Facet const* const>(       \
                                 ElemsName)) {}                                \
                                                                               \
        std::span<ElemType const* const> ElemsName() const {                   \
            return detail::unsafeSpanCast<ElemType const* const>(              \
                NonTerminalFacet::children());                                 \
        }                                                                      \
    };

/// Generic list facet
PRISM_DEFINE_LIST_FACET(ListFacet, Facet, elems)

/// Statement list facet
PRISM_DEFINE_LIST_FACET(StmtListFacet, StmtFacet, elems)

/// Parameter list facet
PRISM_DEFINE_LIST_FACET(ParamListFacet, ParamDeclFacet, elems)

/// Generic parameter list facet
PRISM_DEFINE_LIST_FACET(GenParamListFacet, GenParamDeclFacet, elems)

/// Base list facet
PRISM_DEFINE_LIST_FACET(BaseListFacet, BaseDeclFacet, elems)

/// Member list facet
PRISM_DEFINE_LIST_FACET(MemberListFacet, DeclFacet, elems)

/// Top level facet, a list of declarations
PRISM_DEFINE_LIST_FACET(SourceFileFacet, DeclFacet, decls)

#undef PRISM_DEFINE_LIST_FACET

class SourceContext;
class TreeFormatter;

struct FacetPrintOptions {
    using CallbackType =
        utl::function_view<void(std::ostream& ostream, Facet const* facet,
                                Facet const* parent, size_t index)>;

    SourceContext const* srcCtx = nullptr;

    CallbackType nodeCallback = {};
};

/// Prints \p facet  as a tree to \p ostream
void print(Facet const* facet, std::ostream& ostream,
           FacetPrintOptions options = {});

/// \overload
void print(Facet const* facet, TreeFormatter& fmt,
           FacetPrintOptions options = {});

} // namespace prism

#include <Prism/Common/MacroUtilsUndef.h>

#include <Prism/Facet/Facets.inl>

#endif // PRISM_FACET_FACET_H
