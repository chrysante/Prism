#include "Prism/Sema2/SemaPrint.h"

#include <iostream>

#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/stack.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;
using namespace tfmt::modifiers;

using ranges::views::enumerate;
using ranges::views::reverse;

namespace {

static constexpr utl::streammanip Keyword = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(Bold | BrightMagenta, args...);
};

static constexpr utl::streammanip Username = [](std::ostream& str,
                                                auto const&... args) {
    str << tfmt::format(BrightBlue, args...);
};

static constexpr utl::streammanip Secondary = [](std::ostream& str,
                                                 auto const&... args) {
    str << tfmt::format(BrightGrey, args...);
};

static constexpr utl::streammanip Comment = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(BrightGrey | Italic, "// ", args...);
};

static constexpr utl::streammanip Null = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
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

static void fmt_name(Symbol const* symbol, std::ostream& str,
                     FmtNameOptions options = {});

static void fmt_gen_args(std::span<Symbol const* const> args, std::ostream& str,
                         std::string_view open_paren = "(",
                         std::string_view close_paren = ")") {
    if (args.empty()) return;
    str << open_paren;
    for (bool first = true; auto* arg: args) {
        if (!first) str << ", ";
        first = false;
        fmt_name(arg, str, { .qualified = true });
    }
    str << close_paren;
}

static void fmt_name(Symbol const* symbol, std::ostream& str,
                     FmtNameOptions options) {
    if (!symbol) {
        str << Null;
        return;
    }
    if (options.qualified) {
        utl::stack<Symbol const*> stack = { symbol };
        auto* scope = symbol->parent_scope();
        while (scope) {
            auto* sym = scope->defining_symbol();
            if (isa<SourceFile>(sym) || isa<Module>(sym)) break;
            if (sym) stack.push(sym);
            scope = scope->parent_scope();
        }
        bool first = true;
        for (auto* sym: stack | reverse) {
            if (!first) str << ".";
            first = false;
            fmt_name(sym, str);
        }
        return;
    }
    if (auto* type = dyncast<FunctionType const*>(symbol)) {
        str << Keyword("fn") << " " << "(";
        for (bool first = true; auto arg: type->arguments()) {
            if (!first) str << ", ";
            first = false;
            str << arg.passing_convention() << " ";
            fmt_name(arg.type(), str, options);
        }
        str << ") -> ";
        fmt_name(type->return_type(), str, options);
        return;
    }
    if (auto* type = dyncast<StructInst const*>(symbol)) {
        fmt_name(type->definition(), str, options);
        fmt_gen_args(type->generic_args(), str);
        return;
    }
    if (auto* trait = dyncast<TraitInst const*>(symbol)) {
        fmt_name(trait->definition(), str, options);
        fmt_gen_args(trait->generic_args(), str);
        return;
    }
#if 0
    if (auto* impl = dyncast<TraitImpl const*>(symbol)) {
        str << "(" << Keyword("impl") << " ";
        fmt_name(impl->trait(), str, options);
        str << " " << Keyword("for") << " ";
        fmt_name(impl->conformingType(), str, options);
        str << ")";
        return;
    }
#endif
    std::string_view name = symbol->name();
    if (name.empty()) {
        str << Secondary("anon: ", get_rtti(*symbol));
        return;
    }
    str << name;
}

static auto fmt_name(Symbol const* symbol, FmtNameOptions options = {}) {
    return utl::streammanip(
        [=](std::ostream& str) { fmt_name(symbol, str, options); });
}

static auto fmt_name(Symbol const& symbol, FmtNameOptions options = {}) {
    return fmt_name(&symbol, options);
}

static FmtDeclOptions asSecondary(FmtDeclOptions in) {
    return {
        .primary_qualified = in.secondary_qualified,
        .secondary_qualified = in.secondary_qualified,
    };
}

static FmtNameOptions as_primary_name(FmtDeclOptions in) {
    return { .qualified = in.primary_qualified };
}

static FmtNameOptions as_secondary_name(FmtDeclOptions in) {
    return { .qualified = in.secondary_qualified };
}

static void fmt_decl(Symbol const* symbol, std::ostream& str,
                     FmtDeclOptions options);

static void fmt_decl_impl(Symbol const&, std::ostream& str, FmtDeclOptions) {
    str << tfmt::format(BrightRed | Bold, "<unknown-decl>");
}

static void fmtFuncDeclImpl(Symbol const& func, FunctionDef const& function,
                            std::ostream& str, FmtDeclOptions options) {
    str << fmt_name(func, as_primary_name(options)) << "(";
    for (bool first = true; auto* param: function.arguments()) {
        if (!first) str << ", ";
        first = false;
        fmt_decl(param, str, asSecondary(options));
    }
    str << ") -> "
        << fmt_name(function.return_type(), as_secondary_name(options));
}

static void fmt_decl_impl(FunctionInst const& func, std::ostream& str,
                          FmtDeclOptions options) {
    str << Keyword("fn") << " ";
    fmtFuncDeclImpl(func, *func.definition(), str, options);
}

static void fmt_gen_param_list(std::span<Symbol const* const> params,
                               std::ostream& str, FmtDeclOptions options) {
    str << "[";
    for (bool first = true; auto* param: params) {
        if (!first) str << ", ";
        first = false;
        fmt_decl(param, str, asSecondary(options));
    }
    str << "]";
}
#if 0
static void fmt_decl_impl(FunctionArgument const& arg, std::ostream& str,
                        FmtDeclOptions options) {
    PRISM_UNIMPLEMENTED();
#if 0
    str << fmt_name(arg) << ": " << arg.
    << fmt_name(arg.type().get(), as_secondary_name(options));
#endif
}

static void fmt_decl_impl(GenTypeParam const& param, std::ostream& str,
                        FmtDeclOptions options) {
    str << fmt_name(param) << ": "
    << fmt_name(param.trait_bound(), as_secondary_name(options));
}

static void fmt_decl_impl(StructInst const& type, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("struct") << " " << fmt_name(type, as_primary_name(options));
}

static void fmt_decl_impl(StructDef const& def, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("struct") << " ";
    fmt_gen_param_list(def.generic_params(), str, options);
    str << " " << fmt_name(type, as_primary_name(options));
}

static void fmt_decl_impl(TraitDef const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("trait") << " " << fmt_name(trait, as_primary_name(options));
}

static void fmt_decl_impl(GenTraitInst const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen trait inst") << " "
    << fmt_name(trait, as_primary_name(options));
}

static void fmt_decl_impl(GenTrait const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen trait") << " ";
    fmt_gen_param_list(trait.genParams(), str, options);
    str << " " << fmt_name(trait, as_primary_name(options));
}

static void fmt_decl_impl(TraitImpl const& impl, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("impl") << " "
    << fmt_name(impl.trait(), as_primary_name(options)) << " "
    << Keyword("for") << " "
    << fmt_name(impl.conformingType(), as_secondary_name(options));
}

static void fmt_decl_impl(GenTraitImpl const& impl, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("impl") << " ";
    fmt_gen_param_list(impl.genParams(), str, options);
    str << " " << fmt_name(impl.trait(), as_primary_name(options)) << " "
    << Keyword("for") << " "
    << fmt_name(impl.conformingType(), as_secondary_name(options));
}

static void fmt_decl_impl(Variable const& var, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("var") << " " << fmt_name(var, as_primary_name(options)) << ": "
    << fmt_name(var.type(), as_secondary_name(options));
}

static void fmt_decl_impl(Typedef const& type, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("typedef") << " " << fmt_name(type, as_primary_name(options))
    << ": " << fmt_name(type.traitBound(), as_secondary_name(options)) << " = "
    << fmt_name(type.definition(), as_secondary_name(options));
}

static void fmt_decl_impl(BaseClass const& base, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("base class") << " " << fmt_name(base, as_primary_name(options))
    << ": " << fmt_name(base.type(), as_secondary_name(options));
}

static void fmt_decl_impl(BaseTrait const& base, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("base trait") << " " << fmt_name(base, as_primary_name(options))
    << ": " << fmt_name(base.trait(), as_secondary_name(options));
}

static void fmt_decl_impl(MemberVar const& var, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("member var") << " " << fmt_name(var, as_primary_name(options))
    << ": " << fmt_name(var.type(), as_secondary_name(options));
}
#endif

static void fmt_decl(Symbol const* symbol, std::ostream& str,
                     FmtDeclOptions options = {}) {
    if (!symbol) {
        str << Null;
        return;
    }
    visit(*symbol,
          [&](auto const& symbol) { fmt_decl_impl(symbol, str, options); });
}

static auto fmt_decl(Symbol const* symbol, FmtDeclOptions options = {}) {
    return utl::streammanip(
        [=](std::ostream& str) { fmt_decl(symbol, str, options); });
}

static auto fmt_decl(Symbol const& symbol, FmtDeclOptions options = {}) {
    return fmt_decl(&symbol, options);
}

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

    void do_print(BindingInst const& inst) {
        str << ValueName(inst.initializer());
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

utl::vstreammanip<> prism::format_decl(Symbol const& symbol,
                                       FmtDeclOptions options) {
    return [&, options](std::ostream& str) { fmt_decl(&symbol, str, options); };
}

utl::vstreammanip<> prism::format_decl(Symbol const* symbol,
                                       FmtDeclOptions options) {
    return [=](std::ostream& str) { fmt_decl(symbol, str, options); };
}

utl::vstreammanip<> prism::format_name(Symbol const& symbol,
                                       FmtNameOptions options) {
    return [&, options](std::ostream& str) { fmt_name(&symbol, str, options); };
}

utl::vstreammanip<> prism::format_name(Symbol const* symbol,
                                       FmtNameOptions options) {
    return [=](std::ostream& str) { fmt_name(symbol, str, options); };
}
