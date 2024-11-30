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
#include <Prism/Source/Token.h>

namespace prism {

enum class ParseTreeNodeType {
#define PARSETREE_NODE(Type, ...) Type,
#include <Prism/ParseTree/ParseTree.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(ParseTreeNodeType)

#define PARSETREE_NODE(Type, ...) class ParseTree##Type;
#include <Prism/ParseTree/ParseTree.def>

using ParseTreeNoParent = void;

} // namespace prism

#define PARSETREE_NODE(Type, Parent, Corporeality)                             \
    CSP_DEFINE(prism::ParseTree##Type, prism::ParseTreeNodeType::Type,         \
               prism::ParseTree##Parent, Corporeality)
#include <Prism/ParseTree/ParseTree.def>

namespace prism {

class alignas(void*) ParseTreeNode {
public:
    std::span<ParseTreeNode const* const> children() const {
        return { getChildrenPtr(), getNumChildren() };
    }

    ParseTreeNode const* childAt(size_t index) const {
        PRISM_ASSERT(index < getNumChildren(), "Index out of bounds");
        return getChildrenPtr()[index];
    }

protected:
    explicit ParseTreeNode(Token tok): term{ .tok = tok } {}

    explicit ParseTreeNode(ParseTreeNodeType nodeType,
                           std::span<ParseTreeNode const* const> children):
        nonTerm{ .nonTermFlag = true,
                 .numChildren = static_cast<uint32_t>(children.size()),
                 .type = nodeType } {
        std::copy(children.begin(), children.end(), getChildrenPtr());
    }

    friend ParseTreeNodeType get_rtti(ParseTreeNode const& node) {
        return node.getNodeType();
    }

    ParseTreeNodeType getNodeType() const {
        return isTerminal() ? ParseTreeNodeType::Terminal : nonTerm.type;
    }

    bool isTerminal() const {
        return (std::bit_cast<uint64_t>(*this) & 1) == 0;
    }

    ParseTreeNode const** getChildrenPtr() const {
        return (ParseTreeNode const**)((unsigned char*)this + sizeof(void*));
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
            ParseTreeNodeType type;
        } nonTerm;
    };
};

static_assert(sizeof(ParseTreeNode) == sizeof(void*));
static_assert(alignof(ParseTreeNode) == alignof(void*));

class ParseTreeTerminal: public ParseTreeNode {
public:
    std::span<ParseTreeNode const* const> children() const { return {}; }

    Token token() const { return term.tok; }

private:
    friend ParseTreeNodeType get_rtti(ParseTreeTerminal const& node) {
        return ParseTreeNodeType::Terminal;
    }

    friend class ParseTreeContext;
    ParseTreeTerminal(Token tok): ParseTreeNode(tok) {}
};

class ParseTreeNonTerminal: public ParseTreeNode {
    std::span<ParseTreeNode const* const> children() const {
        return { getChildrenPtr(), nonTerm.numChildren };
    }

protected:
    ParseTreeNonTerminal(ParseTreeNodeType type,
                         std::span<ParseTreeNode const* const> children):
        ParseTreeNode(type, children) {}

private:
    friend ParseTreeNodeType get_rtti(ParseTreeNonTerminal const& node) {
        return node.nonTerm.type;
    }
};

#define PARSE_TREE_FIELD(Index, Type, Name)                                    \
    ParseTree##Type const* Name() const {                                      \
        return csp::cast<ParseTree##Type const*>(childAt(Index));              \
    }

class ParseTreeCastFacet: public ParseTreeNonTerminal {
public:
    PARSE_TREE_FIELD(0, Node, operand)
    PARSE_TREE_FIELD(1, Terminal, operation)
    PARSE_TREE_FIELD(2, Node, target)

private:
    friend class ParseTreeContext;
    explicit ParseTreeCastFacet(std::span<ParseTreeNode const* const> args):
        ParseTreeNonTerminal(ParseTreeNodeType::CastFacet, args) {}
};

class ParseTreeCondFacet: public ParseTreeNonTerminal {
public:
    PARSE_TREE_FIELD(0, Node, condition)
    PARSE_TREE_FIELD(1, Terminal, question)
    PARSE_TREE_FIELD(2, Node, ifFacet)
    PARSE_TREE_FIELD(3, Terminal, colon)
    PARSE_TREE_FIELD(4, Node, thenFacet)

private:
    friend class ParseTreeContext;
    explicit ParseTreeCondFacet(std::span<ParseTreeNode const* const> args):
        ParseTreeNonTerminal(ParseTreeNodeType::CondFacet, args) {}
};

class ParseTreeBinaryFacet: public ParseTreeNonTerminal {
public:
    PARSE_TREE_FIELD(0, Node, LHS)
    PARSE_TREE_FIELD(1, Terminal, operation)
    PARSE_TREE_FIELD(2, Node, RHS)

private:
    friend class ParseTreeContext;
    explicit ParseTreeBinaryFacet(std::span<ParseTreeNode const* const> args):
        ParseTreeNonTerminal(ParseTreeNodeType::BinaryFacet, args) {}
};

class ParseTreePrefixFacet: public ParseTreeNonTerminal {
public:
    PARSE_TREE_FIELD(0, Terminal, operation)
    PARSE_TREE_FIELD(1, Node, operand)

private:
    friend class ParseTreeContext;
    explicit ParseTreePrefixFacet(std::span<ParseTreeNode const* const> args):
        ParseTreeNonTerminal(ParseTreeNodeType::PrefixFacet, args) {}
};

class ParseTreePostfixFacet: public ParseTreeNonTerminal {
public:
    PARSE_TREE_FIELD(0, Node, operand)
    PARSE_TREE_FIELD(1, Terminal, operation)

private:
    friend class ParseTreeContext;
    explicit ParseTreePostfixFacet(std::span<ParseTreeNode const* const> args):
        ParseTreeNonTerminal(ParseTreeNodeType::PostfixFacet, args) {}
};

class ParseTreeListFacet: public ParseTreeNonTerminal {
private:
    friend class ParseTreeContext;
    explicit ParseTreeListFacet(std::span<ParseTreeNode const* const> children):
        ParseTreeNonTerminal(ParseTreeNodeType::ListFacet, children) {}
};

class ParseTreeCallFacet: public ParseTreeNonTerminal {
public:
    PARSE_TREE_FIELD(0, Node, callee)
    PARSE_TREE_FIELD(1, Terminal, openBracket)
    PARSE_TREE_FIELD(2, ListFacet, arguments)
    PARSE_TREE_FIELD(3, Terminal, closeBracket)

private:
    friend class ParseTreeContext;
    explicit ParseTreeCallFacet(std::span<ParseTreeNode const* const> args):
        ParseTreeNonTerminal(ParseTreeNodeType::CallFacet, args) {}
};

class ParseTreeContext {
public:
    ParseTreeTerminal const* terminal(Token tok) {
        void* buf = allocate();
        return new (buf) ParseTreeTerminal(tok);
    }

    ParseTreeCastFacet const* castFacet(ParseTreeNode const* operand,
                                        Token operation,
                                        ParseTreeNode const* target) {
        return nonTerminal<ParseTreeCastFacet>(
            { { operand, terminal(operation), target } });
    }

    ParseTreeCondFacet const* condFacet(ParseTreeNode const* condition,
                                        Token question,
                                        ParseTreeNode const* ifFacet,
                                        Token colon,
                                        ParseTreeNode const* thenFacet) {
        return nonTerminal<ParseTreeCondFacet>(
            { { condition, terminal(question), ifFacet, terminal(colon),
                thenFacet } });
    }

    ParseTreeBinaryFacet const* binaryFacet(ParseTreeNode const* lhs,
                                            Token operation,
                                            ParseTreeNode const* rhs) {
        return nonTerminal<ParseTreeBinaryFacet>(
            { { lhs, terminal(operation), rhs } });
    }

    ParseTreePrefixFacet const* prefixFacet(Token operation,
                                            ParseTreeNode const* operand) {
        return nonTerminal<ParseTreePrefixFacet>(
            { { terminal(operation), operand } });
    }

    ParseTreePostfixFacet const* postfixFacet(ParseTreeNode const* operand,
                                              Token operation) {
        return nonTerminal<ParseTreePostfixFacet>(
            { { operand, terminal(operation) } });
    }

    ParseTreeListFacet const* listFacet(
        std::span<ParseTreeNode const* const> children) {
        return nonTerminal<ParseTreeListFacet>(children);
    }

    ParseTreeCallFacet const* callFacet(ParseTreeNode const* callee,
                                        Token openBracket,
                                        ParseTreeListFacet const* arguments,
                                        Token closeBracket) {
        return nonTerminal<ParseTreeCallFacet>(
            { { callee, terminal(openBracket), arguments,
                terminal(closeBracket) } });
    }

private:
    template <typename T>
    T const* nonTerminal(std::span<ParseTreeNode const* const> children) {
        void* buf = allocate(children.size());
        return new (buf) T(children);
    }

    void* allocate();
    void* allocate(size_t numChildren);

    MonotonicBufferAllocator alloc;
};

class TreeFormatter;

///
void print(ParseTreeNode const* root, std::ostream& ostream);

/// \overload
void print(ParseTreeNode const* root, std::ostream& ostream,
           TreeFormatter& fmt);

} // namespace prism

#endif // PRISM_PARSETREE_PARSETREE_H
