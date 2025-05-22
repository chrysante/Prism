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
                                          Symbol const& symbol) {
    tfmt::FormatGuard guard(Italic, str);
    str << "\"";
    auto* s = &symbol;
    utl::stack<Symbol const*> stack = { s };
    while (true) {
        auto* scope = s->scope();
        if (!scope) break;
        do
            scope = scope->parent_scope();
        while (scope && !scope->defining_symbol());
        s = scope ? scope->defining_symbol() : nullptr;
        if (!s || isa<SourceFile>(s) || isa<Module>(s)) break;
        stack.push(s);
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
        str << SymTypeStyle(*symbol) << " " << NameStyle(*symbol) << " ";
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

    void writeDetails(Symbol const&) {}
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& ostr) {
    TreeFormatter treeFmt(ostr);
    Print2Ctx ctx{ ostr, treeFmt };
    ctx.print(&symbol);
}

void prism::print(Symbol const& symbol) { print(symbol, std::cerr); }
