#ifndef PRISM_PARSETREE_PARSETREE_H
#define PRISM_PARSETREE_PARSETREE_H

#include <algorithm>
#include <bit>
#include <iosfwd>
#include <span>

#include <csp.hpp>
#include <utl/ipp.hpp>

#include <Prism/Common/Allocator.h>
#include <Prism/Common/Assert.h>
#include <Prism/Common/EnumUtil.h>
#include <Prism/Common/NoParent.h>
#include <Prism/Source/Token.h>

namespace prism {

enum class FacetType {
#define PARSETREE_NODE(Type, ...) Type,
#include <Prism/ParseTree/ParseTree.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(FacetType)

#define PARSETREE_NODE(Type, ...) class Type;
#include <Prism/ParseTree/ParseTree.def>

} // namespace prism

#define PARSETREE_NODE(Type, Parent, Corporeality)                             \
    CSP_DEFINE(prism::Type, prism::FacetType::Type, prism::Parent, Corporeality)
#include <Prism/ParseTree/ParseTree.def>

namespace prism {

class alignas(void*) Facet {
public:
    std::span<Facet const* const> children() const {
        return { getChildrenPtr(), getNumChildren() };
    }

    Facet const* childAt(size_t index) const {
        PRISM_ASSERT(index < getNumChildren(), "Index out of bounds");
        return getChildrenPtr()[index];
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
        return (Facet const**)((unsigned char*)this + sizeof(void*));
    };

    size_t getNumChildren() const {
        return isTerminal() ? 0 : nonTerm.numChildren;
    }

    union {
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

class TerminalFacet: public Facet {
public:
    std::span<Facet const* const> children() const { return {}; }

    Token token() const { return term.tok; }

private:
    friend FacetType get_rtti(TerminalFacet const& node) {
        return FacetType::TerminalFacet;
    }

    friend class ParseTreeContext;
    TerminalFacet(Token tok): Facet(tok) {}
};

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

#define PARSE_TREE_FIELD(Index, Type, Name)                                    \
    Type const* Name() const { return csp::cast<Type const*>(childAt(Index)); }

class CastFacet: public NonTerminalFacet {
public:
    PARSE_TREE_FIELD(0, Facet, operand)
    PARSE_TREE_FIELD(1, TerminalFacet, operation)
    PARSE_TREE_FIELD(2, Facet, target)

private:
    friend class ParseTreeContext;
    explicit CastFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::CastFacet, args) {}
};

class CondFacet: public NonTerminalFacet {
public:
    PARSE_TREE_FIELD(0, Facet, condition)
    PARSE_TREE_FIELD(1, TerminalFacet, question)
    PARSE_TREE_FIELD(2, Facet, ifFacet)
    PARSE_TREE_FIELD(3, TerminalFacet, colon)
    PARSE_TREE_FIELD(4, Facet, thenFacet)

private:
    friend class ParseTreeContext;
    explicit CondFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::CondFacet, args) {}
};

class BinaryFacet: public NonTerminalFacet {
public:
    PARSE_TREE_FIELD(0, Facet, LHS)
    PARSE_TREE_FIELD(1, TerminalFacet, operation)
    PARSE_TREE_FIELD(2, Facet, RHS)

private:
    friend class ParseTreeContext;
    explicit BinaryFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::BinaryFacet, args) {}
};

class PrefixFacet: public NonTerminalFacet {
public:
    PARSE_TREE_FIELD(0, TerminalFacet, operation)
    PARSE_TREE_FIELD(1, Facet, operand)

private:
    friend class ParseTreeContext;
    explicit PrefixFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::PrefixFacet, args) {}
};

class PostfixFacet: public NonTerminalFacet {
public:
    PARSE_TREE_FIELD(0, Facet, operand)
    PARSE_TREE_FIELD(1, TerminalFacet, operation)

private:
    friend class ParseTreeContext;
    explicit PostfixFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::PostfixFacet, args) {}
};

class ListFacet: public NonTerminalFacet {
private:
    friend class ParseTreeContext;
    explicit ListFacet(std::span<Facet const* const> children):
        NonTerminalFacet(FacetType::ListFacet, children) {}
};

class CallFacet: public NonTerminalFacet {
public:
    PARSE_TREE_FIELD(0, Facet, callee)
    PARSE_TREE_FIELD(1, TerminalFacet, openBracket)
    PARSE_TREE_FIELD(2, ListFacet, arguments)
    PARSE_TREE_FIELD(3, TerminalFacet, closeBracket)

private:
    friend class ParseTreeContext;
    explicit CallFacet(std::span<Facet const* const> args):
        NonTerminalFacet(FacetType::CallFacet, args) {}
};

class ParseTreeContext {
public:
    TerminalFacet const* terminal(Token tok) {
        void* buf = allocate();
        return new (buf) TerminalFacet(tok);
    }

    CastFacet const* castFacet(Facet const* operand, Token operation,
                               Facet const* target) {
        return nonTerminal<CastFacet>(
            { { operand, terminal(operation), target } });
    }

    CondFacet const* condFacet(Facet const* condition, Token question,
                               Facet const* ifFacet, Token colon,
                               Facet const* thenFacet) {
        return nonTerminal<CondFacet>(
            { { condition, terminal(question), ifFacet, terminal(colon),
                thenFacet } });
    }

    BinaryFacet const* binaryFacet(Facet const* lhs, Token operation,
                                   Facet const* rhs) {
        return nonTerminal<BinaryFacet>({ { lhs, terminal(operation), rhs } });
    }

    PrefixFacet const* prefixFacet(Token operation, Facet const* operand) {
        return nonTerminal<PrefixFacet>({ { terminal(operation), operand } });
    }

    PostfixFacet const* postfixFacet(Facet const* operand, Token operation) {
        return nonTerminal<PostfixFacet>({ { operand, terminal(operation) } });
    }

    ListFacet const* listFacet(std::span<Facet const* const> children) {
        return nonTerminal<ListFacet>(children);
    }

    CallFacet const* callFacet(Facet const* callee, Token openBracket,
                               ListFacet const* arguments, Token closeBracket) {
        return nonTerminal<CallFacet>(
            { { callee, terminal(openBracket), arguments,
                terminal(closeBracket) } });
    }

private:
    template <typename T>
    T const* nonTerminal(std::span<Facet const* const> children) {
        void* buf = allocate(children.size());
        return new (buf) T(children);
    }

    void* allocate();
    void* allocate(size_t numChildren);

    MonotonicBufferAllocator alloc;
};

class TreeFormatter;

///
void print(Facet const* root, std::ostream& ostream);

/// \overload
void print(Facet const* root, std::ostream& ostream, TreeFormatter& fmt);

} // namespace prism

#endif // PRISM_PARSETREE_PARSETREE_H
