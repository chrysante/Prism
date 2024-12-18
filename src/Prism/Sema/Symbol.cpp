#include "Prism/Sema/Symbol.h"

#include <ostream>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

#include "Prism/Common/IndentingStreambuf.h"

using namespace prism;
using ranges::views::enumerate;

Scope const* Symbol::associatedScope() const {
    return visit<Scope const*>(*this, []<typename S>(S const& sym) {
        if constexpr (std::is_base_of_v<detail::AssocScope, S>)
            return sym.detail::AssocScope::associatedScope();
        else
            return nullptr;
    });
}

namespace {

using namespace tfmt::modifiers;

static constexpr utl::streammanip Keyword = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(Bold | BrightMagenta, args...);
};

static constexpr utl::streammanip Comment = [](std::ostream& str,
                                               auto const&... args) {
    str << tfmt::format(BrightGrey, "// ", args...);
};

static constexpr utl::streammanip Null = [](std::ostream& str) {
    str << "<" << tfmt::format(BrightRed | Bold, "NULL") << ">";
};

struct SymbolPrinter {
    std::ostream& str;
    IndentingStreambuf<> buf;
    OstreamBufferGuard guard;

    explicit SymbolPrinter(std::ostream& str):
        str(str), buf(str.rdbuf(), {}), guard(str, &buf) {}

    auto print(Symbol const* symbol) {
        return utl::streammanip([=, this](std::ostream& str) {
            PRISM_ASSERT(&this->str == &str);
            if (!symbol)
                str << Null;
            else
                visit(*symbol, [&](auto const& symbol) { printImpl(symbol); });
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
            str << print(sym);
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

    void printImpl(Function const& func) {
        str << Keyword("fn") << " " << func.name() << ": " << print(func.type())
            << " ";
        printBraced(func.associatedScope());
    }

    void printImpl(CompositeType const& type) {
        // clang-format off
        auto keyword = visit(type, csp::overload {
            [](StructType const&) { return "struct"; },
            // [](TraitType const&) { return "trait"; },
        }); // clang-format on
        str << Keyword(keyword) << " " << type.name() << " ";
        printBraced(type.associatedScope());
    }

    void printImpl(Variable const& var) {
        str << Keyword("var") << " " << var.name() << ": " << print(var.type());
    }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str) {
    str << SymbolPrinter(str).print(&symbol) << '\n';
}
