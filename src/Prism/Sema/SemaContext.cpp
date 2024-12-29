#include "Prism/Sema/SemaContext.h"

#include <bit>

#include "Prism/Sema/Symbol.h"

using namespace prism;

SemaContext::SemaContext() {
    builtins.resize(magic_enum::enum_count<BuiltinSymbol>());
}

SemaContext::~SemaContext() = default;

#define SEMA_BUILTIN(Name, Spelling, SymType)                                  \
    SymType* SemaContext::get##Name() const {                                  \
        return cast<SymType*>(getBuiltin(BuiltinSymbol::Name));                \
    }
#include <Prism/Sema/Builtins.def>

template <typename KeyType, typename T>
static T getOrMake(utl::hashmap<KeyType, T>& map, auto&& key, auto&& ctor) {
    auto itr = map.find(key);
    if (itr != map.end()) return itr->second;
    auto result = ctor();
    map.insert({ key, result });
    return result;
}

ReferenceType const* SemaContext::getRefType(QualType referred) {
    return getOrMake(refTypes, std::bit_cast<uintptr_t>(referred),
                     [&] { return make<ReferenceType>(referred); });
}

DynTraitType const* SemaContext::getDynTraitType(Trait* trait) {
    return getOrMake(dynTraitTypes, trait,
                     [&] { return make<DynTraitType>(trait); });
}

Symbol* SemaContext::addSymbol(csp::unique_ptr<Symbol> sym) {
    auto* s = sym.get();
    symbolBag.push_back(std::move(sym));
    return s;
}
