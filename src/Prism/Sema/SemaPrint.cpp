#include "Prism/Sema/SemaPrint.h"

#include <iostream>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/stack.hpp>

#include "Prism/Common/IndentingStreambuf.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using namespace tfmt::modifiers;
using ranges::views::concat;
using ranges::views::enumerate;
using ranges::views::filter;
using ranges::views::intersperse;
using ranges::views::reverse;
using ranges::views::transform;

static constexpr utl::streammanip Keyword = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(Bold | BrightMagenta, args...);
};

static constexpr utl::streammanip Username = [](std::ostream& str,
                                                auto const&... args) {
    str << tfmt::format(BrightGreen, args...);
};

static constexpr utl::streammanip Comment = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(BrightGrey, "// ", args...);
};

static constexpr utl::streammanip Decorated =
    [](std::ostream& str, tfmt::Modifier const& mod, auto const& leftDelim,
       auto const& rightDelim, auto const&... args) {
    str << tfmt::format(mod, leftDelim);
    ((str << args), ...);
    str << tfmt::format(mod, rightDelim);
};

static constexpr utl::streammanip Null = [](std::ostream& str) {
    str << tfmt::format(BrightRed | Bold, "NULL");
};

static bool isUser(Symbol const& sym) {
    return isa<UserType>(sym) || isa<Trait>(sym);
}

namespace {

struct FmtNameOptions {
    bool qualified = false;
};

} // namespace
static void fmtName(QualType type, std::ostream& str,
                    FmtNameOptions options = {});
static void fmtName(Symbol const* symbol, std::ostream& str,
                    FmtNameOptions options = {});

static void fmtName(QualType type, std::ostream& str, FmtNameOptions options) {
    if (type.isMut()) str << Keyword("mut") << " ";
    fmtName(type.get(), str, options);
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
            if (isa<SourceFile>(sym)) break;
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
        str << tfmt::format(BrightGrey, "anon: ", get_rtti(*symbol));
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

namespace {

struct FmtDeclOptions {
    bool primaryQualified = false;
    bool secondaryQualified = false;

    FmtDeclOptions asSecondary() const {
        return {
            .primaryQualified = secondaryQualified,
            .secondaryQualified = secondaryQualified,
        };
    }

    FmtNameOptions asPrimaryName() const {
        return { .qualified = primaryQualified };
    }

    FmtNameOptions asSecondaryName() const {
        return { .qualified = secondaryQualified };
    }
};

} // namespace

static void fmtDecl(Symbol const* symbol, std::ostream& str,
                    FmtDeclOptions options);

static void fmtDeclImpl(Symbol const& symbol, std::ostream& str,
                        FmtDeclOptions options) {
    str << "<invalid-decl>";
}

static void fmtDeclImpl(Function const& func, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("fn") << " " << fmtName(func, options.asPrimaryName())
        << "(";
    for (bool first = true; auto* param: func.params()) {
        if (!first) str << ", ";
        first = false;
        fmtDecl(param, str, options.asSecondary());
    }
    str << ") -> " << fmtName(func.retType(), options.asSecondaryName());
}

static void fmtDeclImpl(FuncParam const& param, std::ostream& str,
                        FmtDeclOptions options) {
    str << fmtName(param) << ": "
        << fmtName(param.type(), options.asSecondaryName());
}

static void fmtDeclImpl(StructType const& type, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("struct") << " " << fmtName(type, options.asPrimaryName());
}

static void fmtDeclImpl(Trait const& trait, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("trait") << " " << fmtName(trait, options.asPrimaryName());
}

static void fmtDeclImpl(TraitImpl const& impl, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("impl") << " "
        << fmtName(impl.trait(), options.asPrimaryName()) << " "
        << Keyword("for") << " "
        << fmtName(impl.conformingType(), options.asSecondaryName());
}

static void fmtDeclImpl(Variable const& var, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("var") << " " << fmtName(var, options.asPrimaryName())
        << ": " << fmtName(var.type(), options.asSecondaryName());
}

static void fmtDeclImpl(BaseClass const& base, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("base_class") << " "
        << fmtName(base, options.asPrimaryName()) << ": "
        << fmtName(base.type(), options.asSecondaryName());
}

static void fmtDeclImpl(BaseTrait const& base, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("base_trait") << " "
        << fmtName(base, options.asPrimaryName()) << ": "
        << fmtName(base.trait(), options.asSecondaryName());
}

static void fmtDeclImpl(MemberVar const& var, std::ostream& str,
                        FmtDeclOptions options) {
    str << Keyword("memvar") << " " << fmtName(var, options.asPrimaryName())
        << ": " << fmtName(var.type(), options.asSecondaryName());
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
        printChildren(builtins, { "\n", "\n\n" });
        printChildren(userDefined, DeclOpt);
    }

    void printImpl(SourceFile const& file) {
        str << Comment(file.name()) << "\n\n";
        printChildren(file.associatedScope(), DeclOpt);
    }

    auto valueDecl(Value const& value) {
        return utl::streammanip([&](std::ostream& str) {
            str << fmtName(value) << ": " << fmtName(value.type().get()) << " "
                << tfmt::format(BrightGrey, value.cat());
        });
    }

    void printImpl(Value const& value) { str << valueDecl(value); }

    void printImpl(FuncParam const& param) {
        str << fmtName(param) << ": " << fmtName(param.type());
    }

    void printImpl(Function const& func) {
        str << fmtDecl(func) << " ";
        if (isa<FunctionImpl>(func)) printBraced(func.associatedScope());
    }

    void printImpl(Type const& type) { str << fmtName(type); }

    void printLayout(Type const& type) {
        buf.indented([&] { str << Comment(type.layout()); });
    }

    void printObligation(Obligation const& obl) {
        str << fmtDecl(obl.symbol(), { .primaryQualified = true }) << "\n";
    }

    void printObligations(TraitLike const& trait) {
        buf.indented([&] {
            str << "Obligations:\n";
            buf.indented([&] {
                for (auto* obl: trait.obligations())
                    printObligation(*obl);
            });
        });
    }

    void printConformance(Conformance const& conf) {
        str << fmtDecl(conf.symbol(), { .primaryQualified = true }) << " for "
            << fmtDecl(conf.obligation()->symbol(),
                       { .primaryQualified = true })
            << "\n";
    }

    void printConformances(ImplLike const& impl) {
        buf.indented([&] {
            str << "Conformances:\n";
            buf.indented([&] {
                for (auto* conf: impl.conformances())
                    printConformance(*conf);
            });
        });
    }

    template <typename T>
    void printRequirements(T const& symbol) {
        auto nextLine = [this, done = false]() mutable {
            if (done) return;
            str << "\n";
            done = true;
        };
        if constexpr (std::derived_from<T, TraitLike>) {
            if (options.traitObligations) {
                nextLine();
                printObligations(static_cast<TraitLike const&>(symbol));
            }
        }
        if constexpr (std::derived_from<T, ImplLike>) {
            if (options.traitConformances) {
                nextLine();
                printConformances(static_cast<ImplLike const&>(symbol));
            }
        }
    }

    void printImpl(CompositeType const& type) {
        str << fmtDecl(type) << " ";
        if (options.structureMemoryLayout) {
            printLayout(type);
        }
        printRequirements(type);
        // We jump through some hoops here to always print the base classes and
        // non-static membar variables in the order of declaration, and all
        // other symbols afterwards in arbitrary order
        auto members =
            concat(type.bases() | transform(cast<MemberSymbol const*>),
                   type.memberVars());
        auto memSet = members | ranges::to<utl::hashset<Symbol const*>>;
        auto others = type.associatedScope()->symbols() |
                      filter(FN1(&, !memSet.contains(_1)));
        printBraced(concat(members, others));
    }

    void printImpl(Trait const& trait) {
        str << fmtDecl(trait) << " ";
        printRequirements(trait);
        printBraced(trait.associatedScope());
    }

    void printImpl(TraitImpl const& impl) {
        str << fmtDecl(impl) << " ";
        printRequirements(impl);
        printBraced(impl.associatedScope());
    }

    void printImpl(Variable const& var) { str << fmtDecl(var) << " "; }

    void printImpl(MemberSymbol const& member) { str << fmtDecl(member); }

    void printImpl(BaseTrait const& base) { str << fmtDecl(base); }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str,
                  SemaPrintOptions options) {
    SymbolPrinter(str, options).print(&symbol);
}

void prism::print(Symbol const& symbol) { print(symbol, std::cerr); }

utl::vstreammanip<> prism::formatDecl(Symbol const& symbol) {
    return [&](std::ostream& str) { fmtDecl(&symbol, str); };
}

utl::vstreammanip<> prism::formatDecl(Symbol const* symbol) {
    return [=](std::ostream& str) { fmtDecl(symbol, str); };
}

utl::vstreammanip<> prism::formatName(Symbol const& symbol) {
    return [&](std::ostream& str) { fmtName(&symbol, str); };
}

utl::vstreammanip<> prism::formatName(Symbol const* symbol) {
    return [=](std::ostream& str) { fmtName(symbol, str); };
}
