#include <span>
#include <string_view>
#include <variant>

#include <utl/vector.hpp>

#include <Prism/Sema/SymbolFwd.h>

namespace prism {

class Scope;

class NameLookupResult {
    using OverloadSet = utl::small_vector<Function*>;
    using AmbiSet = utl::small_vector<Symbol*>;
    using None = std::monostate;

public:
    NameLookupResult() = default;

    NameLookupResult(Symbol* symbol): data(symbol) {}

    NameLookupResult(OverloadSet overloadSet): data(std::move(overloadSet)) {}

    NameLookupResult(AmbiSet ambiSet): data(std::move(ambiSet)) {}

    bool isNone() const { return is<None>(); }

    bool isSingleSymbol() const { return is<Symbol*>(); }

    Symbol* singleSymbol() const { return get<Symbol*>(); }

    bool isOverloadSet() const { return is<OverloadSet>(); }

    std::span<Function* const> overloadSet() const {
        return get<OverloadSet>();
    }

    bool isAmbiguous() const { return is<Symbol*>(); }

    std::span<Symbol* const> ambiguousSymbols() const { return get<AmbiSet>(); }

private:
    template <typename T>
    bool is() const {
        return std::holds_alternative<T>(data);
    }
    template <typename T>
    T const& get() const {
        return std::get<T>(data);
    }

    std::variant<None, Symbol*, OverloadSet, AmbiSet> data;
};

NameLookupResult unqualifiedLookup(Scope* scope, std::string_view name);

} // namespace prism
