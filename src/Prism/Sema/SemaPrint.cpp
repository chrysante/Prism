#include "Prism/Sema/SemaPrint.h"

#include <iostream>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/stack.hpp>

#include "Prism/Common/IndentingStreambuf.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Common/TreeFormatter.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using namespace tfmt::modifiers;
using ranges::views::concat;
using ranges::views::enumerate;
using ranges::views::filter;
using ranges::views::intersperse;
using ranges::views::reverse;
using ranges::views::transform;

template <typename T, typename... U>
concept DerivedFromAny = (std::derived_from<T, U> || ...);

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

static bool isUser(Symbol const& sym) {
    return isa<UserType>(sym) || isa<Trait>(sym);
}

static void fmtName(QualType type, std::ostream& str,
                    FmtNameOptions options = {});
static void fmtName(Symbol const* symbol, std::ostream& str,
                    FmtNameOptions options = {});

static void fmtName(QualType type, std::ostream& str, FmtNameOptions options) {
    if (type.isMut()) str << Keyword("mut") << " ";
    fmtName(type.get(), str, options);
}

static void fmtGenArgs(std::span<Symbol const* const> args, std::ostream& str,
                       std::string_view openParen = "(",
                       std::string_view closeParen = ")") {
    str << openParen;
    for (bool first = true; auto* arg: args) {
        if (!first) str << ", ";
        first = false;
        fmtName(arg, str, { .qualified = true });
    }
    str << closeParen;
}

static void fmtName(Symbol const* symbol, std::ostream& str,
                    FmtNameOptions options) {
    if (!symbol) {
        str << Null;
        return;
    }
    if (options.qualified) {
        utl::stack<Symbol const*> stack = { symbol };
        auto* scope = symbol->parentScope();
        while (scope) {
            auto* sym = scope->assocSymbol();
            if (isa<SourceFile>(sym) || isa<Target>(sym)) break;
            if (sym) stack.push(sym);
            scope = scope->parent();
        }
        bool first = true;
        for (auto* sym: stack | reverse) {
            if (!first) str << ".";
            first = false;
            fmtName(sym, str);
        }
        return;
    }
    if (auto* type = dyncast<GenStructTypeInst const*>(symbol)) {
        fmtName(type->genTemplate(), str, options);
        fmtGenArgs(type->genArguments(), str);
        return;
    }
    if (auto* trait = dyncast<GenTraitInst const*>(symbol)) {
        fmtName(trait->genTemplate(), str, options);
        fmtGenArgs(trait->genArguments(), str);
        return;
    }
    if (auto* ref = dyncast<ReferenceType const*>(symbol)) {
        str << "&";
        fmtName(ref->referred(), str, options);
        return;
    }
    if (auto* dynType = dyncast<DynType const*>(symbol)) {
        str << Keyword("dyn") << " ";
        fmtName(dynType->underlyingSymbol(), str, options);
        return;
    }
    std::string_view name = symbol->name();
    if (name.empty()) {
        str << Secondary("anon: ", get_rtti(*symbol));
        return;
    }
    if (isBuiltinSymbol(*symbol)) {
        str << Keyword(name);
        return;
    }
    if (isUser(*symbol)) {
        str << Username(name);
        return;
    }
    str << name;
}

static auto fmtName(QualType type, FmtNameOptions options) {
    return utl::streammanip(
        [=](std::ostream& str) { fmtName(type, str, options); });
}

static auto fmtName(Symbol const* symbol, FmtNameOptions options = {}) {
    return utl::streammanip(
        [=](std::ostream& str) { fmtName(symbol, str, options); });
}

static auto fmtName(Symbol const& symbol, FmtNameOptions options = {}) {
    return fmtName(&symbol, options);
}

static FmtDeclOptions asSecondary(FmtDeclOptions in) {
    return {
        .primaryQualified = in.secondaryQualified,
        .secondaryQualified = in.secondaryQualified,
    };
}

static FmtNameOptions asPrimaryName(FmtDeclOptions in) {
    return { .qualified = in.primaryQualified };
}

static FmtNameOptions asSecondaryName(FmtDeclOptions in) {
    return { .qualified = in.secondaryQualified };
}

static void fmtDecl(Symbol const* symbol, std::ostream& str,
                    FmtDeclOptions options);

static void fmtDeclImpl(Symbol const&, std::ostream& str, FmtDeclOptions) {
    str << tfmt::format(BrightRed | Bold, "<unknown-decl>");
}

static void fmtFuncDeclImpl(Symbol const& func, FuncInterface const& interface,
                            std::ostream& str, FmtDeclOptions options) {
    str << fmtName(func, asPrimaryName(options)) << "(";
    for (bool first = true; auto* param: interface.params()) {
        if (!first) str << ", ";
        first = false;
        fmtDecl(param, str, asSecondary(options));
    }
    str << ") -> " << fmtName(interface.retType(), asSecondaryName(options));
}

static void fmtDeclImpl(Function const& func, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("fn") << " ";
    fmtFuncDeclImpl(func, func.interface(), str, options);
}

static void fmtGenParamList(std::span<Symbol const* const> params,
                            std::ostream& str, FmtDeclOptions options) {
    str << "[";
    for (bool first = true; auto* param: params) {
        if (!first) str << ", ";
        first = false;
        fmtDecl(param, str, asSecondary(options));
    }
    str << "]";
}

static void fmtDeclImpl(GenFuncImpl const& func, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen fn") << " ";
    fmtGenParamList(func.genParams(), str, options);
    str << " ";
    fmtFuncDeclImpl(func, func.interface(), str, options);
}

static void fmtDeclImpl(FuncParam const& param, std::ostream& str,
                        FmtDeclOptions options) {
    str << fmtName(param) << ": "
        << fmtName(param.type(), asSecondaryName(options));
}

static void fmtDeclImpl(GenericTypeParam const& param, std::ostream& str,
                        FmtDeclOptions options) {
    str << fmtName(param) << ": "
        << fmtName(param.traitBound(), asSecondaryName(options));
}

static void fmtDeclImpl(StructType const& type, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("struct") << " " << fmtName(type, asPrimaryName(options));
}

static void fmtDeclImpl(GenStructTypeInst const& type, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen struct inst") << " "
        << fmtName(type, asPrimaryName(options));
}

static void fmtDeclImpl(GenStructType const& type, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen struct") << " ";
    fmtGenParamList(type.genParams(), str, options);
    str << " " << fmtName(type, asPrimaryName(options));
}

static void fmtDeclImpl(TraitDef const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("trait") << " " << fmtName(trait, asPrimaryName(options));
}

static void fmtDeclImpl(GenTraitInst const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen trait inst") << " "
        << fmtName(trait, asPrimaryName(options));
}

static void fmtDeclImpl(GenTrait const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("gen trait") << " ";
    fmtGenParamList(trait.genParams(), str, options);
    str << " " << fmtName(trait, asPrimaryName(options));
}

static void fmtDeclImpl(TraitImpl const& impl, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("impl") << " "
        << fmtName(impl.trait(), asPrimaryName(options)) << " "
        << Keyword("for") << " "
        << fmtName(impl.conformingType(), asSecondaryName(options));
}

static void fmtDeclImpl(GenTraitImpl const& impl, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("impl") << " ";
    fmtGenParamList(impl.genParams(), str, options);
    str << " " << fmtName(impl.trait(), asPrimaryName(options)) << " "
        << Keyword("for") << " "
        << fmtName(impl.conformingType(), asSecondaryName(options));
}

static void fmtDeclImpl(Variable const& var, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("var") << " " << fmtName(var, asPrimaryName(options)) << ": "
        << fmtName(var.type(), asSecondaryName(options));
}

static void fmtDeclImpl(BaseClass const& base, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("base class") << " " << fmtName(base, asPrimaryName(options))
        << ": " << fmtName(base.type(), asSecondaryName(options));
}

static void fmtDeclImpl(BaseTrait const& base, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("base trait") << " " << fmtName(base, asPrimaryName(options))
        << ": " << fmtName(base.trait(), asSecondaryName(options));
}

static void fmtDeclImpl(MemberVar const& var, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("member var") << " " << fmtName(var, asPrimaryName(options))
        << ": " << fmtName(var.type(), asSecondaryName(options));
}

static void fmtDecl(Symbol const* symbol, std::ostream& str,
                    FmtDeclOptions options = {}) {
    if (!symbol) {
        str << Null;
        return;
    }
    visit(*symbol,
          [&](auto const& symbol) { fmtDeclImpl(symbol, str, options); });
}

static auto fmtDecl(Symbol const* symbol, FmtDeclOptions options = {}) {
    return utl::streammanip(
        [=](std::ostream& str) { fmtDecl(symbol, str, options); });
}

static auto fmtDecl(Symbol const& symbol, FmtDeclOptions options = {}) {
    return fmtDecl(&symbol, options);
}

namespace {

struct SymbolPrinter {
    std::ostream& str;
    SemaPrintOptions options;
    IndentingStreambuf<> buf;
    OstreamBufferGuard guard;

    explicit SymbolPrinter(std::ostream& str, SemaPrintOptions options):
        str(str), options(options), buf(str.rdbuf(), {}), guard(str, &buf) {}

    void print(Symbol const* symbol) {
        if (!symbol)
            str << Null;
        else
            visit(*symbol, [&](auto const& symbol) { printImpl(symbol); });
    }

    struct PrintChildrenOptions {
        std::string_view separator, separatorAfterLast;
    };

    static constexpr PrintChildrenOptions StmtOpt = {
        .separator = "\n", .separatorAfterLast = "\n"
    };

    static constexpr PrintChildrenOptions DeclOpt = {
        .separator = "\n\n", .separatorAfterLast = "\n"
    };

    void printChildren(auto&& symbols, PrintChildrenOptions opt) {
        bool first = true;
        for (auto* sym: symbols) {
            if (!first) str << opt.separator;
            first = false;
            print(sym);
        }
        if (!first) str << opt.separatorAfterLast;
    }

    void printChildren(Scope const* scope, PrintChildrenOptions opt) {
        if (!scope) return;
        printChildren(scope->symbols(), opt);
    }

    void printBraced(auto&& symbols) {
        if (ranges::empty(symbols)) {
            str << "{}";
            return;
        }
        str << "{\n";
        buf.indented([&] { printChildren(symbols, StmtOpt); });
        str << "}";
    }

    void printBraced(Scope const* scope) {
        if (!scope)
            str << "{ " << Null << " }";
        else
            printBraced(scope->symbols());
    }

    void printImpl(Symbol const& symbol) { str << fmtName(symbol); }

    void printImpl(Target const& target) {
        auto* scope = target.associatedScope();
        utl::small_vector<Symbol const*> builtins, userDefined;
        for (auto* sym: scope->symbols()) {
            (isBuiltinSymbol(*sym) ? builtins : userDefined).push_back(sym);
        }
        str << Keyword("target") << " " << target.name() << "\n\n";
        str << Comment("Builtins:") << "\n";
        for (bool first = true; auto* builtin: builtins) {
            if (!first) str << ", ";
            first = false;
            str << fmtName(builtin);
        }
        str << "\n\n";
        printChildren(userDefined, DeclOpt);
    }

    void printImpl(Library const& lib) {
        str << Keyword("library") << " " << lib.name() << " ";
        printBraced(lib.associatedScope());
    }

    void printImpl(SourceFile const& file) {
        str << Comment("Source file \"", file.name(), "\":") << "\n\n";
        printChildren(file.associatedScope(), DeclOpt);
    }

    auto valueDecl(Value const& value) {
        return utl::streammanip([&](std::ostream& str) {
            str << fmtName(value) << ": " << fmtName(value.type().get()) << " "
                << Secondary(value.cat());
        });
    }

    void printImpl(Value const& value) { str << valueDecl(value); }

    void printImpl(GenericTypeParam const& param) {
        str << Keyword("genparam") << " " << fmtName(param) << ": "
            << fmtName(param.traitBound());
    }

    void printImpl(FuncParam const& param) {
        str << fmtName(param) << ": " << fmtName(param.type());
    }

    void printImpl(Function const& func) {
        str << fmtDecl(func) << " ";
        if (isa<FunctionImpl>(func)) printBraced(func.associatedScope());
    }

    void printImpl(DerivedFromAny<FunctionImpl, GenFuncImpl> auto const& func) {
        str << fmtDecl(func) << " ";
        printBraced(func.associatedScope());
    }

    void printImpl(Type const& type) { str << fmtName(type); }

    void printLayout(Type const& type) {
        buf.indented([&] { str << Comment(type.layout()) << " "; });
    }

    void printObligation(Obligation const& obl) {
        str << fmtDecl(obl.symbol(), { .primaryQualified = true });
        if (obl.conformances().empty()) {
            str << " " << tfmt::format(BrightYellow | Italic, "[unmatched]")
                << "\n";
            return;
        }
        auto color = obl.singleConformance() ? BrightGreen : BrightYellow;
        str << "\n";
        buf.indented([&] {
            for (auto* sym: obl.conformances())
                str << tfmt::format(color | Italic, "matched by:") << " "
                    << fmtDecl(sym, { .primaryQualified = true }) << "\n";
        });
    }

    void printObligations(InterfaceLike const& interface) {
        buf.indented([&] {
            for (auto& [key, list]: interface.obligations())
                for (auto* obl: list)
                    printObligation(*obl);
        });
    }

    void printRequirements(InterfaceLike const& interface) {
        if (options.traitObligations) {
            str << "\n";
            printObligations(interface);
        }
    }

    template <DerivedFromAny<CompositeType, GenCompositeType> T>
    void printImpl(T const& type) {
        str << fmtDecl(type) << " ";
        if constexpr (std::derived_from<T, CompositeType>)
            if (options.structureMemoryLayout) printLayout(type);
        printRequirements(type);
        printBraced(type.associatedScope());
    }

    void printImpl(DerivedFromAny<Trait, GenTrait> auto const& trait) {
        str << fmtDecl(trait) << " ";
        printRequirements(trait);
        printBraced(trait.associatedScope());
    }

    void printImpl(DerivedFromAny<TraitImpl, GenTraitImpl> auto const& impl) {
        str << fmtDecl(impl) << " ";
        printRequirements(impl);
        printBraced(impl.associatedScope());
    }

    void printImpl(Variable const& var) { str << fmtDecl(var) << " "; }

    void printImpl(MemberSymbol const& member) { str << fmtDecl(member); }

    void printImpl(BaseTrait const& base) { str << fmtDecl(base); }

    void printImpl(IntLiteral const& lit) {
        str << tfmt::format(tfmt::Yellow, lit.valueAsString());
    }

    void printImpl(RetInst const& ret) {
        str << Keyword("return") << " ";
        if (ret.retval()) print(ret.retval());
    }
};

struct ScopeHierarchyPrinter {
    std::ostream& str;
    TreeFormatter& fmt;

    void print(Scope const* scope) {
        if (!scope) {
            str << Null;
            return;
        }
        fmt.writeChildren(scope->symbols(), [&](Symbol const* sym) {
            std::string_view name = sym->name();
            str << name;
            if (!name.empty()) str << ": ";
            str << Secondary(get_rtti(*sym)) << "\n";
            if (auto* scope = sym->associatedScope()) print(scope);
        });
    }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str,
                  SemaPrintOptions options) {
    SymbolPrinter(str, options).print(&symbol);
}

void prism::print(Symbol const& symbol) { print(symbol, std::cerr); }

void prism::printScopeHierarchy(Scope const* scope, std::ostream& str) {
    TreeFormatter fmt(str);
    ScopeHierarchyPrinter(str, fmt).print(scope);
}

void prism::printScopeHierarchy(Scope const* scope) {
    printScopeHierarchy(scope, std::cerr);
}

utl::vstreammanip<> prism::formatDecl(Symbol const& symbol,
                                      FmtDeclOptions options) {
    return [&, options](std::ostream& str) { fmtDecl(&symbol, str, options); };
}

utl::vstreammanip<> prism::formatDecl(Symbol const* symbol,
                                      FmtDeclOptions options) {
    return [=](std::ostream& str) { fmtDecl(symbol, str, options); };
}

utl::vstreammanip<> prism::formatName(Symbol const& symbol,
                                      FmtNameOptions options) {
    return [&, options](std::ostream& str) { fmtName(&symbol, str, options); };
}

utl::vstreammanip<> prism::formatName(Symbol const* symbol,
                                      FmtNameOptions options) {
    return [=](std::ostream& str) { fmtName(symbol, str, options); };
}
