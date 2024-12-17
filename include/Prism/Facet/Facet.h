#ifndef PRISM_FACET_FACET_H
#define PRISM_FACET_FACET_H

#include <algorithm>
#include <bit>
#include <iosfwd>
#include <span>

#include <range/v3/range_concepts.hpp>
#include <range/v3/size.hpp>

#include <Prism/Common/Allocator.h>
#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>
#include <Prism/Facet/FacetFwd.h>
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

    friend FacetType get_rtti(Facet const& node) { return node.getNodeType(); }

    FacetType getNodeType() const {
        auto cat = getCategory();
        return cat == FacetType::NonTerminalFacet ? nonTerm.type : cat;
    }

    FacetType getCategory() const {
        using enum FacetType;
        size_t index = std::bit_cast<uint64_t>(*this) & 1;
        return std::array{ TerminalFacet, NonTerminalFacet }[index];
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
    Token token() const { return term.tok; }

private:
    friend FacetType get_rtti(TerminalFacet const& node) {
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
        return node.nonTerm.type;
    }
};

/// Generic list facet
class ListFacet: public NonTerminalFacet {
public:
    explicit ListFacet(std::span<Facet const* const> params):
        NonTerminalFacet(FacetType::ListFacet, params) {}
};

/// Statement list facet
class StmtListFacet: public NonTerminalFacet {
public:
    explicit StmtListFacet(std::span<StmtFacet const* const> params):
        NonTerminalFacet(FacetType::StmtListFacet,
                         detail::unsafeSpanCast<Facet const* const>(params)) {}

    std::span<StmtFacet const* const> elems() const {
        return detail::unsafeSpanCast<StmtFacet const* const>(
            NonTerminalFacet::children());
    }
};

/// Parameter list facet
class ParamListFacet: public NonTerminalFacet {
public:
    explicit ParamListFacet(std::span<ParamDeclFacet const* const> params):
        NonTerminalFacet(FacetType::ParamListFacet,
                         detail::unsafeSpanCast<Facet const* const>(params)) {}

    std::span<ParamDeclFacet const* const> elems() const {
        return detail::unsafeSpanCast<ParamDeclFacet const* const>(
            NonTerminalFacet::children());
    }
};

/// Top level facet, a list of declarations
class SourceFileFacet: public NonTerminalFacet {
public:
    explicit SourceFileFacet(std::span<DeclFacet const* const> params):
        NonTerminalFacet(FacetType::SourceFileFacet,
                         detail::unsafeSpanCast<Facet const* const>(params)) {}

    std::span<DeclFacet const* const> decls() const {
        return detail::unsafeSpanCast<DeclFacet const* const>(
            NonTerminalFacet::children());
    }
};

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

#include <Prism/Facet/Facets.inl>

#endif // PRISM_FACET_FACET_H
