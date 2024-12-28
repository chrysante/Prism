#include "Prism/Sema/SemaPrint.h"

#include <ostream>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>

#include "Prism/Common/IndentingStreambuf.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using namespace tfmt::modifiers;
using ranges::views::enumerate;
using ranges::views::intersperse;
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

static void fmtNameImpl(QualType type, std::ostream& str);
static void fmtNameImpl(Symbol const* symbol, std::ostream& str);

static void fmtNameImpl(QualType type, std::ostream& str) {
    if (type.isMut()) str << Keyword("mut") << " ";
    fmtNameImpl(type.get(), str);
}

static void fmtNameImpl(Symbol const* symbol, std::ostream& str) {
    if (!symbol) {
        str << Null;
        return;
    }
    if (auto* ref = dyncast<ReferenceType const*>(symbol)) {
        str << "&";
        fmtNameImpl(ref->referred(), str);
        return;
    }
    if (auto* dynType = dyncast<DynType const*>(symbol)) {
        str << Keyword("dyn") << " ";
        fmtNameImpl(dynType->underlyingSymbol(), str);
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

static auto fmtName(QualType type) {
    return utl::streammanip([=](std::ostream& str) { fmtNameImpl(type, str); });
}

static auto fmtName(Symbol const* symbol) {
    return utl::streammanip(
        [=](std::ostream& str) { fmtNameImpl(symbol, str); });
}

static auto fmtName(Symbol const& symbol) { return fmtName(&symbol); }

static void fmtDecl(Symbol const* symbol, std::ostream& str);

static void fmtDeclImpl(Symbol const& symbol, std::ostream& str) {
    str << "<invalid-decl>";
}

static void fmtDeclImpl(Function const& func, std::ostream& str) {
    str << Keyword("fn") << " " << fmtName(func) << "(";
    for (bool first = true; auto* param: func.params()) {
        if (!first) str << ", ";
        first = false;
        fmtDecl(param, str);
    }
    str << ") -> " << fmtName(func.retType());
}

static void fmtDeclImpl(FuncParam const& param, std::ostream& str) {
    str << fmtName(param) << ": " << fmtName(param.type());
}

static void fmtDeclImpl(StructType const& type, std::ostream& str) {
    str << Keyword("struct") << " " << fmtName(type);
}

static void fmtDeclImpl(Trait const& trait, std::ostream& str) {
    str << Keyword("trait") << " " << fmtName(trait);
}

static void fmtDeclImpl(TraitImpl const& impl, std::ostream& str) {
    str << Keyword("impl") << " " << fmtName(impl.trait()) << " "
        << Keyword("for") << " " << fmtName(impl.conformingType());
}

static void fmtDeclImpl(Variable const& var, std::ostream& str) {
    str << Keyword("var") << " " << fmtName(var) << ": " << fmtName(var.type());
}

static void fmtDeclImpl(BaseClass const& base, std::ostream& str) {
    str << Keyword("base") << " " << fmtName(base) << ": "
        << fmtName(base.type());
}

static void fmtDeclImpl(MemberVar const& var, std::ostream& str) {
    str << Keyword("memvar") << " " << fmtName(var) << ": "
        << fmtName(var.type());
}

static void fmtDecl(Symbol const* symbol, std::ostream& str) {
    if (!symbol) {
        str << Null;
        return;
    }
    visit(*symbol, [&](auto const& symbol) { fmtDeclImpl(symbol, str); });
}

static auto fmtDecl(Symbol const* symbol) {
    return utl::streammanip([=](std::ostream& str) { fmtDecl(symbol, str); });
}

static auto fmtDecl(Symbol const& symbol) { return fmtDecl(&symbol); }

namespace {

struct SymbolPrinter {
    std::ostream& str;
    IndentingStreambuf<> buf;
    OstreamBufferGuard guard;

    explicit SymbolPrinter(std::ostream& str):
        str(str), buf(str.rdbuf(), {}), guard(str, &buf) {}

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

    void printChildren(std::span<Symbol const* const> symbols,
                       PrintChildrenOptions opt) {
        for (auto [index, sym]: symbols | enumerate) {
            print(sym);
            if (index < symbols.size() - 1)
                str << opt.separator;
            else
                str << opt.separatorAfterLast;
        }
    }

    void printChildren(Scope const* scope, PrintChildrenOptions opt) {
        if (!scope) return;
        printChildren(scope->symbols(), opt);
    }

    void printBraced(Scope const* scope) {
        if (!scope) {
            str << "{ " << Null << " }";
        }
        else if (scope->symbols().empty()) {
            str << "{}";
        }
        else {
            str << "{\n";
            buf.indended([&] { printChildren(scope, StmtOpt); });
            str << "}";
        }
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

    void printImpl(UserType const& type) {
        str << fmtDecl(type) << " ";
        printBraced(type.associatedScope());
    }

    void printImpl(Trait const& trait) {
        str << fmtDecl(trait) << " ";
        printBraced(trait.associatedScope());
    }

    void printImpl(TraitImpl const& impl) {
        str << fmtDecl(impl) << " ";
        printBraced(impl.associatedScope());
    }

    void printImpl(Variable const& var) { str << fmtDecl(var) << " "; }

    void printImpl(MemberSymbol const& member) { str << fmtDecl(member); }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str) {
    SymbolPrinter(str).print(&symbol);
}

utl::vstreammanip<> prism::formatDecl(Symbol const& symbol) {
    return [&](std::ostream& str) { fmtDecl(&symbol, str); };
}
