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

enum class ParseTreeNodeKind {
#define PARSETREE_NODE(Kind) Kind,
#include <Prism/ParseTree/ParseTree.def>
};

PRISM_DEFINE_ENUM_FUNCTIONS(ParseTreeNodeKind)

enum class ParseTreeNodeID { Terminal, NonTerminal, Base };

class ParseTreeNode;
class ParseTreeTerminal;
class ParseTreeNonTerminal;

} // namespace prism

CSP_DEFINE(prism::ParseTreeNode, prism::ParseTreeNodeID::Base, void, Abstract)
CSP_DEFINE(prism::ParseTreeTerminal, prism::ParseTreeNodeID::Terminal,
           prism::ParseTreeNode, Concrete)
CSP_DEFINE(prism::ParseTreeNonTerminal, prism::ParseTreeNodeID::NonTerminal,
           prism::ParseTreeNode, Concrete)

namespace prism {

class alignas(void*) ParseTreeNode {
public:
    ParseTreeNodeKind kind() const { return getKind(); }

    std::span<ParseTreeNode const* const> children() const {
        if (getID() == ParseTreeNodeID::Terminal) {
            return {};
        }
        else {
            return getChildren();
        }
    }

    ParseTreeNode const* childAt(size_t index) const {
        auto c = children();
        PRISM_ASSERT(index < c.size(), "Index out of bounds");
        return c[0];
    }

protected:
    explicit ParseTreeNode(Token tok): tok{ tok } {}

    explicit ParseTreeNode(ParseTreeNodeKind kind,
                           std::span<ParseTreeNode* const> children):
        inner{ 1, static_cast<uint32_t>(children.size()), kind } {
        auto* p = getChildren().data();
        std::copy(children.begin(), children.end(),
                  const_cast<ParseTreeNode**>(p));
    }

    friend ParseTreeNodeID get_rtti(ParseTreeNode const& node) {
        return node.getID();
    }

    ParseTreeNodeKind getKindTerm() const {
        return std::bit_cast<ParseTreeNodeKind>(tok.kind);
    }

    ParseTreeNodeKind getKind() const {
        if (getID() == ParseTreeNodeID::Terminal) {
            return getKindTerm();
        }
        else {
            return inner.kind;
        }
    }

    ParseTreeNodeID getID() const {
        return ParseTreeNodeID(std::bit_cast<uint64_t>(*this) & 1);
    }

    std::span<ParseTreeNode* const> getChildren() const {
        PRISM_ASSERT(getID() == ParseTreeNodeID::NonTerminal);
        auto* ptr = (ParseTreeNode**)((unsigned char*)this + sizeof(void*));
        return { ptr, inner.count };
    };

    union {
        Token tok;
        struct {
            uint32_t flag  : 1;
            uint32_t count : 31;
            ParseTreeNodeKind kind;
        } inner;
    };
};

static_assert(sizeof(ParseTreeNode) == sizeof(void*));
static_assert(alignof(ParseTreeNode) == alignof(void*));

class ParseTreeTerminal: public ParseTreeNode {
public:
    ParseTreeNodeKind kind() const { return getKindTerm(); }

    std::span<ParseTreeNode const* const> children() const { return {}; }

    Token token() const { return tok; }

private:
    friend class ParseTreeContext;

    ParseTreeTerminal(Token tok): ParseTreeNode(tok) {}
};

class ParseTreeNonTerminal: public ParseTreeNode {
    ParseTreeNodeKind kind() const { return inner.kind; }

    std::span<ParseTreeNode const* const> children() const {
        return getChildren();
    }

private:
    friend class ParseTreeContext;

    ParseTreeNonTerminal(ParseTreeNodeKind kind,
                         std::span<ParseTreeNode* const> children):
        ParseTreeNode(kind, children) {}
};

class ParseTreeContext {
public:
    ParseTreeTerminal* makeTerminal(Token tok) {
        void* buf = allocate();
        return new (buf) ParseTreeTerminal(tok);
    }

    ParseTreeNonTerminal* makeNonTerminal(
        ParseTreeNodeKind kind, std::span<ParseTreeNode* const> children) {
        void* buf = allocate(children.size());
        return new (buf) ParseTreeNonTerminal(kind, children);
    }

    ParseTreeNode const* root() const { return r; }

    void setRoot(ParseTreeNode const* root) { r = root; }

private:
    void* allocate();
    void* allocate(size_t numChildren);

    MonotonicBufferAllocator alloc;
    ParseTreeNode const* r = nullptr;
};

class TreeFormatter;

///
void print(ParseTreeNode const* root, std::ostream& ostream);

/// \overload
void print(ParseTreeNode const* root, std::ostream& ostream,
           TreeFormatter& fmt);

} // namespace prism

#endif // PRISM_PARSETREE_PARSETREE_H
