#include "Prism/Sema2/SemaPrint.h"

#include <iostream>

#include <termfmt/termfmt.h>
#include <utl/stack.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;
using namespace tfmt::modifiers;

namespace {

constexpr utl::streammanip Secondary = [](std::ostream& str,
                                          auto const&... args) {
    tfmt::FormatGuard guard(BrightGrey, str);
    ((str << args), ...);
};

constexpr utl::streammanip SymTypeStyle = [](std::ostream& str,
                                             Symbol const& symbol) {
    str << tfmt::format(BrightBlue, get_rtti(symbol));
};

constexpr utl::streammanip NameStyle = [](std::ostream& str,
                                          Symbol const* symbol) {
    if (!symbol) {
        str << "NULL";
        return;
    }
    tfmt::FormatGuard guard(Italic, str);
    str << "\"";
    utl::stack<Symbol const*> stack = { symbol };
    while (true) {
        auto* scope = symbol->scope();
        if (!scope) break;
        do
            scope = scope->parent_scope();
        while (scope && !scope->defining_symbol());
        symbol = scope ? scope->defining_symbol() : nullptr;
        if (!symbol || isa<SourceFile>(symbol) || isa<Module>(symbol)) break;
        stack.push(symbol);
    }
    str << stack.pop()->name();
    while (!stack.empty())
        str << "." << stack.pop()->name();
    str << "\"";
};

struct Print2Ctx {
    std::ostream& str;
    TreeFormatter& treeFmt;

    void print(Symbol const* symbol) {
        if (!symbol) {
            str << "NULL\n";
            return;
        }
        str << SymTypeStyle(*symbol) << " " << NameStyle(symbol) << " ";
        visit(*symbol, FN1(&, writeHeader(_1)));
        str << "\n";
        auto* scope = symbol->scope();
        bool isLeaf = !scope || scope->symbols().empty();
        treeFmt.writeDetails(isLeaf,
                             FN0(&, visit(*symbol, FN1(&, writeDetails(_1)))));
        if (scope) treeFmt.writeChildren(scope->symbols(), FN1(&, print(_1)));
    }

    void writeHeader(Symbol const&) {}

    void writeHeader(Type const& type) {
        str << Secondary("[", type.layout(), "]");
    }

    void writeHeader(GenTypeParam const& param) {
        str << ": " << NameStyle(param.trait_bound());
    }

    void writeHeader(Value const& value) {
        str << ": " << NameStyle(value.type());
    }

    void writeDetails(Symbol const&) {}
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& ostr) {
    TreeFormatter treeFmt(ostr);
    Print2Ctx ctx{ ostr, treeFmt };
    ctx.print(&symbol);
}

void prism::print(Symbol const& symbol) { print(symbol, std::cerr); }
