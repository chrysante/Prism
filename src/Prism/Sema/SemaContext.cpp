#include "Prism/Sema/SemaContext.h"

#include <bit>

#include <range/v3/algorithm.hpp>
#include <utl/hashtable.hpp>

#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

namespace {

template <typename ArgsContainer>
struct GenInstKeyImpl {
    GenericSymbol* templ;
    ArgsContainer args;

    template <typename Cont = ArgsContainer&&>
    GenInstKeyImpl(GenericSymbol* templ, Cont&& args):
        templ(templ), args(std::forward<Cont>(args)) {}

    template <std::convertible_to<ArgsContainer> ArgsContainerRhs>
    GenInstKeyImpl(GenInstKeyImpl<ArgsContainerRhs> const& rhs):
        templ(rhs.templ), args(rhs.args) {}

    GenInstKeyImpl(GenInstKeyImpl<std::span<Symbol const* const>> rhs):
        templ(rhs.templ), args(rhs.args | ToSmallVector<>) {}

    template <typename ArgsContainerRhs>
    bool operator==(GenInstKeyImpl<ArgsContainerRhs> const& rhs) const {
        return templ == rhs.templ && ranges::equal(args, rhs.args);
    }
};

} // namespace

using GenInstKey = GenInstKeyImpl<utl::small_vector<Symbol const*>>;

using GenInstKeyView = GenInstKeyImpl<std::span<Symbol const* const>>;

template <typename ArgsContainer>
struct std::hash<GenInstKeyImpl<ArgsContainer>> {
    size_t operator()(GenInstKeyImpl<ArgsContainer> const& key) const {
        size_t seed = 0;
        utl::hash_combine(seed, key.templ);
        ranges::for_each(key.args, FN1(&, utl::hash_combine(seed, _1)));
        return seed;
    }
};

struct SemaContext::Impl {
    std::vector<Symbol*> builtins;
    utl::hashmap<uintptr_t, ReferenceType*> refTypes;
    utl::hashmap<Trait*, DynTraitType*> dynTraitTypes;
    utl::hashmap<GenInstKey, Symbol*> genInstSymbols;
    std::vector<csp::unique_ptr<Symbol>> symbolBag;
    std::vector<std::unique_ptr<Scope>> scopeBag;
    utl::hashmap<SourceFileFacet const*, SourceContext const*> sourceContextMap;
};

SemaContext::SemaContext() {
    impl->builtins.resize(magic_enum::enum_count<BuiltinSymbol>());
}

SemaContext::~SemaContext() = default;

Symbol* SemaContext::getBuiltin(BuiltinSymbol builtin) const {
    return impl->builtins[(size_t)builtin];
}

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
    return getOrMake(impl->refTypes, std::bit_cast<uintptr_t>(referred),
                     [&] { return make<ReferenceType>(referred); });
}

DynTraitType const* SemaContext::getDynTraitType(Trait* trait) {
    return getOrMake(impl->dynTraitTypes, trait,
                     [&] { return make<DynTraitType>(trait); });
}

Scope* SemaContext::makeScope(Scope* parent) {
    impl->scopeBag.push_back(std::make_unique<Scope>(parent));
    return impl->scopeBag.back().get();
}

SourceContext const* SemaContext::getSourceContext(Facet const* facet) const {
    while (facet) {
        if (auto* file = dyncast<SourceFileFacet const*>(facet)) {
            auto itr = impl->sourceContextMap.find(file);
            return itr != impl->sourceContextMap.end() ? itr->second : nullptr;
        }
        facet = facet->parent();
    }
    return nullptr;
}

Symbol* SemaContext::addSymbol(csp::unique_ptr<Symbol> sym) {
    auto* s = sym.get();
    impl->symbolBag.push_back(std::move(sym));
    return s;
}

Symbol* SemaContext::assignBuiltin(BuiltinSymbol builtinID, Symbol* symbol) {
    impl->builtins[(size_t)builtinID] = symbol;
    return symbol;
}

Symbol* SemaContext::getGenInstImpl(GenericSymbol* gen,
                                    std::span<Symbol* const> args,
                                    utl::function_view<Symbol*()> factory) {
    return getOrMake(impl->genInstSymbols, GenInstKeyView{ gen, args },
                     factory);
}

void SemaContext::mapSourceToContext(SourceFileFacet const* facet,
                                     SourceContext const* ctx) {
    auto [itr, result] = impl->sourceContextMap.insert({ facet, ctx });
    PRISM_ASSERT(result, "Failed to insert source file. Was it added twice?");
}
