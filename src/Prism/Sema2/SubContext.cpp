#include "Prism/Sema2/SubContext.h"

#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::reverse;
using ranges::views::transform;
using ranges::views::zip;

static utl::hashmap<Symbol*, Symbol*> make_map(
    DeclSymbol const* decl, std::span<Symbol* const> gen_args) {
    PRISM_ASSERT(decl && decl->is_generic());
    PRISM_ASSERT(decl->generic_params().size() == gen_args.size());
    // TODO: Assert that arguments have correct symbol type Value/Type
    return zip(decl->generic_params(), gen_args) |
           ranges::to<utl::hashmap<Symbol*, Symbol*>>;
}

DeclSubContext::DeclSubContext(DeclSymbol* decl,
                               std::span<Symbol* const> gen_args):
    _decl(decl), _map(make_map(decl, gen_args)) {}

Symbol const* DeclSubContext::try_resolve(Symbol const* symbol) const {
    PRISM_ASSERT(isa<GenTypeParam>(symbol) || isa<GenValueParam>(symbol),
                 "this check should be performed by the caller");
    auto itr = _map.find(symbol);
    if (itr != _map.end()) return itr->second;
    return nullptr;
}

Symbol const* SubContext::resolve(Symbol const* symbol) const {
    if (!isa<GenTypeParam>(symbol) && !isa<GenValueParam>(symbol))
        return symbol;
    for (auto& decl_context: _stack | reverse)
        if (auto* result = decl_context.try_resolve(symbol)) return result;
    return symbol;
}

Type const* SubContext::resolve(Type const* type) {
    return cast<Type const*>(resolve(static_cast<Symbol const*>(type)));
}

Value* SubContext::resolve(Value* value) {
    return cast<Value*>(resolve(static_cast<Symbol*>(value)));
}

Value const* SubContext::resolve(Value const* value) {
    return cast<Value const*>(resolve(static_cast<Symbol const*>(value)));
}
