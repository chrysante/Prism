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
                     Trait* trait, CompositeType* conforming):
    Symbol(SymbolType::TraitImpl, "", facet, parent),
    AssocScope(ctx.make<Scope>(parent), this),
    _trait(trait),
    conf(conforming) {}

Function::Function(std::string name, Facet const* facet, Scope* parent,
                   utl::small_vector<FunctionParameter>&& params,
                   Type const* retType):
    Symbol(SymbolType::Function, std::move(name), facet, parent),
    _params(std::move(params)),
    _retType(retType) {}

static std::tuple<ValueType const*, Mutability, ValueCat> destructureType(
    Type const* type) {
    // For now
    return { cast<ValueType const*>(type), Mutability::Const, LValue };
}

static void declareArguments(SemaContext& ctx, Scope* scope,
                             std::span<FunctionParameter const> params) {
    for (auto& param: params) {
        auto [type, mut, valueCat] =
            destructureType(cast<Type const*>(param.typeOrConstraint));
        ctx.make<FuncArg>(param.name, param.facet, scope, type, valueCat);
    }
}

FunctionImpl::FunctionImpl(SemaContext& ctx, std::string name,
                           Facet const* facet, Scope* parent,
                           utl::small_vector<FunctionParameter>&& params,
                           Type const* retType):
    Function(std::move(name), facet, parent, std::move(params), retType),
    AssocScope(ctx.make<Scope>(parent), this) {
    setSymbolType(SymbolType::FunctionImpl);
    declareArguments(ctx, associatedScope(), this->params());
}

using namespace tfmt::modifiers;

namespace {

static constexpr utl::streammanip Keyword = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(Bold | BrightMagenta, args...);
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

    auto printName(Symbol const* symbol) {
        return utl::streammanip([=, this](std::ostream& str) {
            PRISM_ASSERT(&this->str == &str);
            if (!symbol)
                str << Null;
            else if (!symbol->name().empty())
                str << symbol->name();
            else
                str << tfmt::format(BrightGrey, "<anon: ", get_rtti(*symbol),
                                    ">");
        });
    }

    struct PrintChildrenOptions {
        std::string_view separator;
        bool separatorAfterLast = true;
    };

    static constexpr PrintChildrenOptions StmtOpt = {
        .separator = "\n", .separatorAfterLast = true
    };

    static constexpr PrintChildrenOptions DeclOpt = {
        .separator = "\n\n", .separatorAfterLast = false
    };

    void printChildren(Scope const* scope, PrintChildrenOptions opt) {
        if (!scope) return;
        for (auto [index, sym]: scope->symbols() | enumerate) {
            print(sym);
            if (opt.separatorAfterLast || index < scope->symbols().size() - 1)
                str << opt.separator;
        }
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

    void printImpl(Symbol const&) {}

    void printImpl(Target const& target) {
        printChildren(target.associatedScope(), StmtOpt);
    }

    void printImpl(SourceFile const& file) {
        str << Comment(file.name()) << "\n\n";
        printChildren(file.associatedScope(), DeclOpt);
    }

    auto valueDecl(Value const& value) {
        return utl::streammanip([&](std::ostream& str) {
            str << value.name() << ": " << printName(value.type()) << " "
                << tfmt::format(BrightGrey, value.cat());
        });
    }

    void printImpl(Value const& value) { str << valueDecl(value); }

    void printImpl(Function const& func) {
        str << Keyword("fn") << " " << func.name() << "(";
        for (bool first = true; auto& param: func.params()) {
            if (!first) str << ", ";
            first = false;
            str << param.name << ": " << printName(param.typeOrConstraint);
        }
        str << ") -> " << printName(func.retType()) << " ";
        if (isa<FunctionImpl>(func)) printBraced(func.associatedScope());
    }

    void printCompTypeOrTrait(std::string_view decl, auto const& sym) {
        str << Keyword(decl) << " " << sym.name() << " ";
        printBraced(sym.associatedScope());
    }

    void printImpl(CompositeType const& type) {
        // clang-format off
        auto decl = visit(type, csp::overload {
            [](StructType const&) { return "struct"; },
            [](GenStructTypeInst const&) { return "inst struct"; }
        }); // clang-format on
        printCompTypeOrTrait(decl, type);
    }

    void printImpl(Trait const& trait) { printCompTypeOrTrait("trait", trait); }

    void printImpl(TraitImpl const& impl) {
        str << Keyword("impl") << " " << printName(impl.trait())
            << Keyword(" for ") << printName(impl.conformingType()) << " ";
        printBraced(impl.associatedScope());
    }

    void printImpl(Variable const& var) {
        str << Keyword("var") << " " << var.name() << ": "
            << printName(var.type());
    }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str) {
    SymbolPrinter(str).print(&symbol);
}
