#ifndef PRISM_AST_FACET_H
#define PRISM_AST_FACET_H

#include <algorithm>
#include <bit>
#include <iosfwd>
#include <span>

#include <csp.hpp>

#include <Prism/Ast/AstFwd.h>
#include <Prism/Common/Allocator.h>
#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/NoParent.h>
#include <Prism/Source/Token.h>

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
        return csp::cast<F const*>(childAt(index));
    }

protected:
    explicit Facet(Token tok): term{ .tok = tok } {}

    explicit Facet(FacetType nodeType, std::span<Facet const* const> children):
        nonTerm{ .nonTermFlag = true,
                 .numChildren = static_cast<uint32_t>(children.size()),
                 .type = nodeType } {
        std::copy(children.begin(), children.end(), getChildrenPtr());
    }

    friend FacetType get_rtti(Facet const& node) { return node.getNodeType(); }

    FacetType getNodeType() const {
        return isTerminal() ? FacetType::TerminalFacet : nonTerm.type;
    }

    bool isTerminal() const {
        return (std::bit_cast<uint64_t>(*this) & 1) == 0;
    }

    Facet const** getChildrenPtr() const {
        return (Facet const**)((unsigned char*)this + sizeof *this);
    };

    size_t getNumChildren() const {
        return isTerminal() ? 0 : nonTerm.numChildren;
    }

    union alignas(void*) {
        struct {
            Token tok;
        } term;
        struct {
            bool nonTermFlag     : 1;
            uint32_t numChildren : 31;
            FacetType type;
        } nonTerm;
    };
};

static_assert(sizeof(Facet) == sizeof(void*));
static_assert(alignof(Facet) == alignof(void*));

namespace detail {

struct FacetFactory {
    static void* allocate(Allocator auto& alloc, size_t numChildren) {
        return alloc.allocate(sizeof(Facet) + numChildren * sizeof(void*),
                              alignof(Facet));
    }

    template <typename T = TerminalFacet>
    static T const* makeTerminal(Allocator auto& alloc, Token tok) {
        void* buf = detail::FacetFactory::allocate(alloc, 0);
        return new (buf) T(tok);
    }

    template <typename T>
    static T const* makeNonTerminal(Allocator auto& alloc,
                                    std::span<Facet const* const> children) {
        void* buf = allocate(alloc, children.size());
        return new (buf) T(children);
    }
};

} // namespace detail

class TerminalFacet: public Facet {
public:
    std::span<Facet const* const> children() const { return {}; }

    Token token() const { return term.tok; }

private:
    friend FacetType get_rtti(TerminalFacet const& node) {
        return FacetType::TerminalFacet;
    }

    friend class detail::FacetFactory;
    TerminalFacet(Token tok): Facet(tok) {}
};

inline TerminalFacet const* makeTerminal(Allocator auto& alloc, Token tok) {
    return detail::FacetFactory::makeTerminal(alloc, tok);
}

class NonTerminalFacet: public Facet {
    std::span<Facet const* const> children() const {
        return { getChildrenPtr(), nonTerm.numChildren };
    }

protected:
    NonTerminalFacet(FacetType type, std::span<Facet const* const> children):
        Facet(type, children) {}

private:
    friend FacetType get_rtti(NonTerminalFacet const& node) {
        return node.nonTerm.type;
    }
};

#define FACET_FIELD(Index, Type, Name)                                         \
    Type const* Name() const { return childAt<Type>(Index); }

class CastFacet: public NonTerminalFacet {
public:
    FACET_FIELD(0, Facet, operand)
    FACET_FIELD(1, TerminalFacet, operation)
    FACET_FIELD(2, Facet, target)

private:
    friend class detail::FacetFactory;
    explicit CastFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::CastFacet, args) {}
};

inline CastFacet const* makeCastFacet(Allocator auto& alloc,
                                      Facet const* operand, Token operation,
                                      Facet const* target) {
    return detail::FacetFactory::makeNonTerminal<CastFacet>(
        alloc, { { operand, makeTerminal(alloc, operation), target } });
}

class CondFacet: public NonTerminalFacet {
public:
    FACET_FIELD(0, Facet, condition)
    FACET_FIELD(1, TerminalFacet, question)
    FACET_FIELD(2, Facet, ifFacet)
    FACET_FIELD(3, TerminalFacet, colon)
    FACET_FIELD(4, Facet, thenFacet)

private:
    friend class detail::FacetFactory;
    explicit CondFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::CondFacet, args) {}
};

inline CondFacet const* makeCondFacet(Allocator auto& alloc,
                                      Facet const* condition, Token question,
                                      Facet const* ifFacet, Token colon,
                                      Facet const* thenFacet) {
    return detail::FacetFactory::makeNonTerminal<CondFacet>(
        alloc, { { condition, makeTerminal(alloc, question), ifFacet,
                   makeTerminal(alloc, colon), thenFacet } });
}

class BinaryFacet: public NonTerminalFacet {
public:
    FACET_FIELD(0, Facet, LHS)
    FACET_FIELD(1, TerminalFacet, operation)
    FACET_FIELD(2, Facet, RHS)

private:
    friend class detail::FacetFactory;
    explicit BinaryFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::BinaryFacet, args) {}
};

inline BinaryFacet const* makeBinaryFacet(Allocator auto& alloc,
                                          Facet const* lhs, Token operation,
                                          Facet const* rhs) {
    return detail::FacetFactory::makeNonTerminal<BinaryFacet>(
        alloc, { { lhs, makeTerminal(alloc, operation), rhs } });
}

class PrefixFacet: public NonTerminalFacet {
public:
    FACET_FIELD(0, TerminalFacet, operation)
    FACET_FIELD(1, Facet, operand)

private:
    friend class detail::FacetFactory;
    explicit PrefixFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::PrefixFacet, args) {}
};

inline PrefixFacet const* makePrefixFacet(Allocator auto& alloc,
                                          Token operation,
                                          Facet const* operand) {
    return detail::FacetFactory::makeNonTerminal<PrefixFacet>(
        alloc, { { makeTerminal(alloc, operation), operand } });
}

class PostfixFacet: public NonTerminalFacet {
public:
    FACET_FIELD(0, Facet, operand)
    FACET_FIELD(1, TerminalFacet, operation)

private:
    friend class detail::FacetFactory;
    explicit PostfixFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::PostfixFacet, args) {}
};

inline PostfixFacet const* makePostfixFacet(Allocator auto& alloc,
                                            Facet const* operand,
                                            Token operation) {
    return detail::FacetFactory::makeNonTerminal<PostfixFacet>(
        alloc, { { operand, makeTerminal(alloc, operation) } });
}

class ListFacet: public NonTerminalFacet {
private:
    friend class detail::FacetFactory;
    explicit ListFacet(std::span<Facet const* const> children):
        NonTerminalFacet(FacetType::ListFacet, children) {}
};

inline ListFacet const* makeListFacet(Allocator auto& alloc,
                                      std::span<Facet const* const> children) {
    return detail::FacetFactory::makeNonTerminal<ListFacet>(alloc, children);
}

class CallFacet: public NonTerminalFacet {
public:
    FACET_FIELD(0, Facet, callee)
    FACET_FIELD(1, TerminalFacet, openBracket)
    FACET_FIELD(2, ListFacet, arguments)
    FACET_FIELD(3, TerminalFacet, closeBracket)

private:
    friend class detail::FacetFactory;
    explicit CallFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::CallFacet, args) {}
};

inline CallFacet const* makeCallFacet(Allocator auto& alloc,
                                      Facet const* callee, Token openBracket,
                                      ListFacet const* arguments,
                                      Token closeBracket) {
    return detail::FacetFactory::makeNonTerminal<CallFacet>(
        alloc, { { callee, makeTerminal(alloc, openBracket), arguments,
                   makeTerminal(alloc, closeBracket) } });
}

#undef FACET_FIELD

class SourceContext;
class TreeFormatter;

/// Prints \p facet  as a tree to \p ostream
void print(Facet const* facet, std::ostream& ostream,
           SourceContext const* srcCtx = nullptr);

/// \overload
void print(Facet const* facet, TreeFormatter& fmt,
           SourceContext const* srcCtx = nullptr);

} // namespace prism

#endif // PRISM_AST_FACET_H
