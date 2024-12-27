#include "Prism/Sema/Symbol.h"

#include <ostream>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/IndentingStreambuf.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"

using namespace prism;
using ranges::views::enumerate;
using ranges::views::intersperse;
using ranges::views::transform;

Symbol::Symbol(SymbolType type, std::string name, Facet const* facet,
               Scope* parent):
    _symType(type), _name(std::move(name)), _facet(facet), _parent(parent) {
    if (parent) parent->addSymbol(this);
}

Scope const* Symbol::associatedScope() const {
    return visit<Scope const*>(*this, []<typename S>(S const& sym) {
        if constexpr (std::is_base_of_v<detail::AssocScope, S>)
            return sym.detail::AssocScope::associatedScope();
        else
            return nullptr;
    });
}

Module::Module(SymbolType type, SemaContext& ctx, std::string name):
    Symbol(type, std::move(name), nullptr, nullptr),
    AssocScope(ctx.make<Scope>(nullptr), this) {}

SourceFile::SourceFile(SemaContext& ctx, std::string name, Facet const* facet,
                       Scope* parent, SourceContext const& sourceCtx):
    Symbol(SymbolType::SourceFile, std::move(name), facet, parent),
    AssocScope(ctx.make<Scope>(parent), this),
    sourceCtx(sourceCtx) {}

CompositeType::CompositeType(SymbolType symType, SemaContext& ctx,
                             std::string name, Facet const* facet,
                             Scope* parent):
    ValueType(symType, std::move(name), facet, parent),
    AssocScope(ctx.make<Scope>(parent), this) {}

Trait::Trait(SemaContext& ctx, std::string name, Facet const* facet,
             Scope* parent):
    Symbol(SymbolType::Trait, std::move(name), facet, parent),
    AssocScope(ctx.make<Scope>(parent), this) {}

TraitImpl::TraitImpl(SemaContext& ctx, Facet const* facet, Scope* parent,
                     Trait* trait, UserType* conforming):
    Symbol(SymbolType::TraitImpl, /* name: */ {}, facet, parent),
    AssocScope(ctx.make<Scope>(parent), this),
    _trait(trait),
    _conf(conforming) {}

Function::Function(std::string name, Facet const* facet, Scope* parent,
                   utl::small_vector<FuncParam*>&& params, Type const* retType):
    Symbol(SymbolType::Function, std::move(name), facet, parent),
    _params(std::move(params)),
    _retType(retType) {}

FunctionImpl::FunctionImpl(SemaContext& ctx, std::string name,
                           Facet const* facet, Scope* parent,
                           utl::small_vector<FuncParam*>&& params,
                           Type const* retType):
    Function(std::move(name), facet, parent, std::move(params), retType),
    AssocScope(ctx.make<Scope>(parent), this) {
    setSymbolType(SymbolType::FunctionImpl);
}

using namespace tfmt::modifiers;

namespace {

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
    str << Decorated(BrightGrey, "<", ">",
                     tfmt::format(BrightRed | Bold, "NULL"));
};

static bool isBuiltin(Symbol const& sym) {
    return isa<VoidType>(sym) || isa<ByteType>(sym) || isa<IntType>(sym) ||
           isa<FloatType>(sym);
}

static bool isUser(Symbol const& sym) {
    return isa<UserType>(sym) || isa<Trait>(sym);
}

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

    void printNameImpl(QualType type) {
        if (type.isMut()) str << Keyword("mut") << " ";
        printNameImpl(type.get());
    }

    void printNameImpl(Symbol const* symbol) {
        PRISM_ASSERT(&this->str == &str);
        if (!symbol) {
            str << Null;
            return;
        }
        if (auto* ref = dyncast<ReferenceType const*>(symbol)) {
            str << "&";
            printNameImpl(ref->referred());
            return;
        }
        if (auto* dynType = dyncast<DynType const*>(symbol)) {
            str << Keyword("dyn") << " ";
            printNameImpl(dynType->underlyingSymbol());
            return;
        }
        std::string_view name = symbol->name();
        if (name.empty()) {
            str << tfmt::format(BrightGrey, "<anon: ", get_rtti(*symbol), ">");
            return;
        }
        if (isBuiltin(*symbol)) {
            str << Keyword(name);
            return;
        }
        if (isUser(*symbol)) {
            str << Username(name);
            return;
        }
        str << name;
    }

    auto printName(QualType type) {
        return utl::streammanip([=, this](auto&) { printNameImpl(type); });
    }

    auto printName(Symbol const* symbol) {
        return utl::streammanip([=, this](auto&) { printNameImpl(symbol); });
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

    void printImpl(Symbol const& symbol) { str << printName(&symbol); }

    void printImpl(Target const& target) {
        auto* scope = target.associatedScope();
        utl::small_vector<Symbol const*> builtins, userDefined;
        for (auto* sym: scope->symbols()) {
            (isBuiltin(*sym) ? builtins : userDefined).push_back(sym);
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
            str << value.name() << ": " << printName(value.type().get()) << " "
                << tfmt::format(BrightGrey, value.cat());
        });
    }

    void printImpl(Value const& value) { str << valueDecl(value); }

    void printImpl(FuncParam const& param) {
        str << param.name() << ": " << printName(param.type());
    }

    void printImpl(Function const& func) {
        str << Keyword("fn") << " " << func.name() << "(";
        for (bool first = true; auto* param: func.params()) {
            if (!first) str << ", ";
            first = false;
            print(param);
        }
        str << ") -> " << printName(func.retType()) << " ";
        if (isa<FunctionImpl>(func)) printBraced(func.associatedScope());
    }

    void printUserTypeOrTrait(std::string_view decl, auto const& sym) {
        str << Keyword(decl) << " " << printName(&sym) << " ";
        printBraced(sym.associatedScope());
    }

    void printImpl(Type const& type) { str << printName(&type); }

    void printImpl(UserType const& type) {
        // clang-format off
        auto decl = visit(type, csp::overload {
            [](StructType const&) { return "struct"; },
            [](GenStructTypeInst const&) { return "inst struct"; }
        }); // clang-format on
        printUserTypeOrTrait(decl, type);
    }

    void printImpl(Trait const& trait) { printUserTypeOrTrait("trait", trait); }

    void printImpl(TraitImpl const& impl) {
        str << Keyword("impl") << " " << printName(impl.trait())
            << Keyword(" for ") << printName(impl.conformingType()) << " ";
        printBraced(impl.associatedScope());
    }

    void printImpl(Variable const& var) {
        str << Keyword("var") << " " << var.name() << ": "
            << printName(var.type());
    }

    void printImpl(BaseClass const& base) {
        str << Keyword("base") << " " << base.name() << ": "
            << printName(base.type());
    }

    void printImpl(MemberVar const& base) {
        str << Keyword("var") << " " << base.name() << ": "
            << printName(base.type());
    }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str) {
    SymbolPrinter(str).print(&symbol);
}
