#ifndef PRISM_PARSER_PARSERTESTUTILS_H
#define PRISM_PARSER_PARSERTESTUTILS_H

#include <concepts>
#include <initializer_list>
#include <optional>
#include <ostream>
#include <span>
#include <variant>
#include <vector>

#include <Prism/Common/Allocator.h>
#include <Prism/Common/Typename.h>
#include <Prism/Facet/Facet.h>
#include <Prism/Source/Token.h>

namespace prism {

namespace internal {

extern MonotonicBufferResource gAlloc;

}

class Diagnostic;

std::ostream& operator<<(std::ostream& str, Facet const& facet);

class ExpectedDiagnostic {
public:
    virtual ~ExpectedDiagnostic() = default;

    virtual bool checkType(Diagnostic const* diag) const = 0;

    uint32_t line;
    std::optional<uint32_t> column;

protected:
    ExpectedDiagnostic(uint32_t line, std::optional<uint32_t> column):
        line(line), column(column) {}

private:
    friend std::ostream& operator<<(std::ostream& str,
                                    ExpectedDiagnostic const& diag) {
        diag.format(str);
        return str;
    }

    virtual void format(std::ostream& str) const = 0;
};

template <typename DiagnosticType>
class ExpectedDiagnosticImpl: public ExpectedDiagnostic {
public:
    explicit ExpectedDiagnosticImpl(uint32_t line,
                                    std::optional<uint32_t> column):
        ExpectedDiagnostic(line, column) {}

    bool checkType(Diagnostic const* diag) const override {
        return dynamic_cast<DiagnosticType const*>(diag) != nullptr;
    }

    void format(std::ostream& str) const override {
        str << "L: " << line << " ";
        if (column) str << "C: " << *column << " ";
        str << getDemangledName<DiagnosticType>();
    }
};

template <std::derived_from<Diagnostic> DiagnosticType>
ExpectedDiagnostic const& DiagnosticOnLine(
    uint32_t line, std::optional<uint32_t> column = std::nullopt) {
    return *allocate<ExpectedDiagnosticImpl<DiagnosticType>>(internal::gAlloc,
                                                             line, column);
}

enum class NullNodeT : int;

inline constexpr NullNodeT NullNode{};

struct VarType: std::variant<FacetType, TokenKind, NullNodeT> {
    using variant::variant;
};

/// Reference tree node for simple and consice testing of parsed ASTs and facet
/// trees
class AstRefNode {
public:
    AstRefNode(VarType type, std::span<AstRefNode const* const> children):
        type(type),
        children(children),
        expectedDiagnostics(&internal::gAlloc) {}

private:
    friend bool operator==(Facet const& facet, AstRefNode const* ref);

    friend AstRefNode* operator>>(AstRefNode* node,
                                  ExpectedDiagnostic const& e);

    bool compare(Facet const* node, Facet const* parent, size_t index) const;

    template <typename T>
    bool checkType(T t) const;

    bool compareChildren(Facet const* node, Facet const* parent,
                         size_t index) const;

    bool verifyDiagnostics() const;

    VarType type;
    std::span<AstRefNode const* const> children;
    std::vector<
        ExpectedDiagnostic const*,
        ResourceAllocator<ExpectedDiagnostic const*, MonotonicBufferResource>>
        expectedDiagnostics;
};

/// Helper class for concise construction of reference trees
class Tree {
public:
    Tree(std::initializer_list<std::variant<AstRefNode const*, VarType>>
             children);

    friend AstRefNode* operator>>(VarType type, Tree children);

private:
    std::span<AstRefNode const* const> value;
};

AstRefNode* operator>>(std::convertible_to<VarType> auto type,
                       std::convertible_to<VarType> auto child) {
    return type >> Tree{ child };
}

SourceFileFacet const* parseFile(std::string_view text);

Facet const* parseExpr(std::string_view text);

Facet const* parseTypeSpec(std::string_view text);

} // namespace prism

#endif // PRISM_PARSER_PARSERTESTUTILS_H
