#include "Prism/Sema2/SemaPrint.h"

#include <iostream>

#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/stack.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;
using namespace tfmt::modifiers;

using ranges::views::enumerate;

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
    if (symbol->name().empty()) {
        str << tfmt::format(BrightGrey | Italic, "<anon>");
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

static constexpr utl::streammanip Keyword = [](std::ostream& str,
                                               std::string_view name) {
    str << tfmt::format(BrightMagenta | Bold, name);
};

static constexpr utl::streammanip ValueName = [](std::ostream& str,
                                                 Value const* value) {
    if (!value)
        str << "NULL";
    else if (!value->name().empty())
        str << "%" << value->name();
    else
        str << tfmt::format(BrightGrey | Italic, "<anon>");
};

static constexpr utl::streammanip TypeName = [](std::ostream& str,
                                                Type const* type) {
    if (!type)
        str << "NULL";
    else if (isa<BuiltinType>(type))
        str << Keyword(type->name());
    else
        str << "%" << type->name();
};

struct PrettyPrintInstCtx {
    std::ostream& str;
    int indent = 0;
    int block_nest_level = 0;
    std::array<tfmt::Modifier, 4> const block_color = { Blue, Cyan, Green,
                                                        Yellow };

    auto bracket_impl(int offset, std::string_view bracket) {
        return utl::streammanip([=, this](std::ostream& str) {
            if (offset < 0) block_nest_level += offset;
            str << tfmt::format(block_color[(unsigned)block_nest_level %
                                            block_color.size()] |
                                    Bold,
                                bracket);
            if (offset > 0) block_nest_level += offset;
        });
    }

    auto open_paren() { return bracket_impl(1, "("); }
    auto close_paren() { return bracket_impl(-1, ")"); }
    auto open_bracket() { return bracket_impl(1, "["); }
    auto close_bracket() { return bracket_impl(-1, "]"); }
    auto open_brace() { return bracket_impl(1, "{"); }
    auto close_brace() { return bracket_impl(-1, "}"); }

    auto begin_line() {
        return utl::streammanip([this](std::ostream& str) {
            for (int i = 0; i < indent; ++i)
                str << "  ";
        });
    }

    void begin_inst(Instruction const& inst) {
        str << begin_line();
        if (!inst.name().empty()) str << ValueName(&inst) << " = ";
    }

    void print(Instruction const* inst) {
        if (!inst) {
            str << begin_line() << "NULL\n";
            return;
        }
        begin_inst(*inst);
        visit(*inst, FN1(&, do_print(_1)));
        str << "\n";
    }

    void do_print(Instruction const&) { PRISM_UNREACHABLE(); }

    void do_print(BlockInst const& block) {
        str << Keyword("block") << " " << TypeName(block.type());
        if (block.empty()) {
            str << " " << open_brace() << close_brace();
            return;
        }
        str << " " << open_brace() << "\n";
        ++indent;
        for (auto* inst: block)
            print(inst);
        --indent;
        str << begin_line() << close_brace();
    }

    void do_print(YieldInst const& inst) {
        str << Keyword("yield") << " " << ValueName(inst.operand());
    }

    void do_print(CallInst const& inst) {
        str << Keyword("call") << " " << TypeName(inst.type()) << " "
            << ValueName(inst.callee()) << open_paren();
        bool first = true;
        for (auto* arg: inst.arguments())
            str << (first ? ((void)(first = false), "") : ", ")
                << ValueName(arg);
        str << close_paren();
    }
};

struct SemaPrintCtx {
    SemaPrintOptions const& options;
    std::ostream& str;
    TreeFormatter& tree_fmt;

    void print(Symbol const* symbol) {
        if (!symbol) {
            str << "NULL\n";
            return;
        }
        if (auto* inst = dyncast<Instruction const*>(symbol);
            inst && options.pretty_print_instructions)
        {
            PrettyPrintInstCtx{ str }.print(inst);
            return;
        }
        str << SymTypeStyle(*symbol) << " " << NameStyle(symbol) << " ";
        visit(*symbol, FN1(&, write_header(_1)));
        str << "\n";
        auto* scope = symbol->scope();
        bool is_leaf = !scope || scope->symbols().empty();
        tree_fmt.writeDetails(is_leaf,
                              FN0(&,
                                  visit(*symbol, FN1(&, write_details(_1)))));
        if (scope) tree_fmt.writeChildren(scope->symbols(), FN1(&, print(_1)));
    }

    void write_header(Symbol const&) {}

    void write_header(Type const& type) {
        str << Secondary("[", type.layout(), "]");
    }

    void write_header(GenTypeParam const& param) {
        str << ": " << NameStyle(param.trait_bound());
    }

    void write_header(Value const& value) {
        str << ": " << NameStyle(value.type());
    }

    void write_details(Symbol const&) {}

    void write_details(Instruction const& inst) {
        for (auto [index, operand]: inst.operands() | enumerate)
            str << "[" << index << "] = " << NameStyle(operand) << "\n";
    }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& ostr,
                  SemaPrintOptions const& options) {
    TreeFormatter tree_fmt(ostr);
    SemaPrintCtx ctx{ options, ostr, tree_fmt };
    ctx.print(&symbol);
}

void prism::print(Symbol const& symbol) { print(symbol, std::cerr); }
