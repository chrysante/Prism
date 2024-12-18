#include "Prism/Sema/Symbol.h"

#include <ostream>

#include <range/v3/algorithm.hpp>
#include <termfmt/termfmt.h>
#include <utl/streammanip.hpp>

using namespace prism;

Scope const* Symbol::associatedScope() const {
    return visit<Scope const*>(*this, []<typename S>(S const& sym) {
        static constexpr auto Base = (Scope const* (Symbol::*)() const) &
                                     Symbol::associatedScope;
        static constexpr auto Derived = (Scope const* (S::*)() const) &
                                        S::associatedScope;
        if constexpr (Base == Derived)
            return nullptr;
        else
            return sym.associatedScope();
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
    str << "// " << tfmt::format(BrightGrey, args...);
};

struct SymbolPrinter {
    std::ostream& str;

    auto print(Symbol const* symbol) {
        return utl::streammanip([=, this](std::ostream& str) {
            PRISM_ASSERT(&this->str == &str);
            if (!symbol) {
                str << "NULL";
            }
            visit(*symbol, [&](auto const& symbol) { printImpl(symbol); });
        });
    }

    void printChildren(Scope const* scope) {
        if (!scope) return;
        ranges::for_each(scope->symbols(),
                         [&](auto* sym) { str << print(sym) << "\n"; });
    }

    void printImpl(Symbol const&) {}

    void printImpl(Target const& target) {
        printChildren(target.associatedScope());
    }

    void printImpl(SourceFile const& file) {
        str << Comment(file.name()) << "\n";
        printChildren(file.associatedScope());
    }

    void printImpl(Function const& func) {
        str << Keyword("fn") << " " << func.name();
    }

    void printImpl(StructType const& type) {
        str << Keyword("struct") << " " << type.name() << "{\n";
        printChildren(type.associatedScope());
    }
};

} // namespace

void prism::print(Symbol const& symbol, std::ostream& str) {
    str << SymbolPrinter{ str }.print(&symbol) << '\n';
}
