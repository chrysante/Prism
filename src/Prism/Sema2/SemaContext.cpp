#include "Prism/Sema2/SemaContext.h"

#include <bit>

#include <range/v3/algorithm.hpp>
#include <utl/hashtable.hpp>

#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

namespace {

struct Builtins {
    BuiltinTrait* type_trait = nullptr;
};

template <typename Def, typename ArgsContainer>
struct GenSpecKeyImpl {
    Def* generic;
    ArgsContainer args;

    template <typename Cont = ArgsContainer&&>
    GenSpecKeyImpl(Def* generic, Cont&& args):
        generic(generic), args(std::forward<Cont>(args)) {}

    template <std::convertible_to<ArgsContainer> ArgsContainerRhs>
    GenSpecKeyImpl(GenSpecKeyImpl<Def, ArgsContainerRhs> const& rhs):
        generic(rhs.generic), args(rhs.args) {}

    GenSpecKeyImpl(GenSpecKeyImpl<Def, std::span<Symbol const* const>> rhs):
        generic(rhs.generic), args(rhs.args | ToSmallVector<>) {}

    template <typename ArgsContainerRhs>
    bool operator==(GenSpecKeyImpl<Def, ArgsContainerRhs> const& rhs) const {
        return generic == rhs.generic && ranges::equal(args, rhs.args);
    }
};

} // namespace

using StructSpecKey =
    GenSpecKeyImpl<StructDef, utl::small_vector<Symbol const*>>;
using StructSpecKeyView =
    GenSpecKeyImpl<StructDef, std::span<Symbol const* const>>;
using TraitSpecKey = GenSpecKeyImpl<TraitDef, utl::small_vector<Symbol const*>>;
using TraitSpecKeyView =
    GenSpecKeyImpl<TraitDef, std::span<Symbol const* const>>;

template <typename Def, typename ArgsContainer>
struct std::hash<GenSpecKeyImpl<Def, ArgsContainer>> {
    size_t operator()(GenSpecKeyImpl<Def, ArgsContainer> const& key) const {
        size_t seed = 0;
        utl::hash_combine(seed, key.generic);
        ranges::for_each(key.args, FN1(&, utl::hash_combine(seed, _1)));
        return seed;
    }
};

struct SemaContext::Impl {
    std::vector<csp::unique_ptr<Symbol>> symbol_bag;
    std::vector<std::unique_ptr<Scope>> scope_bag;
    utl::hashmap<SourceFileFacet const*, SourceContext const*>
        source_context_map;
    utl::hashmap<StructSpecKey, StructType*> struct_specializations;
    utl::hashmap<TraitSpecKey, TraitInst*> trait_specializations;
    Builtins builtins;
};

SemaContext::SemaContext() = default;

SemaContext::~SemaContext() = default;

Module* SemaContext::make_module() {
    auto* mod = make<Module>();
    impl->builtins.type_trait =
        make<BuiltinTrait>(mod->scope(), "type", ScopeArg::make(*this));
    return mod;
}

Scope* SemaContext::make_scope(Symbol* defining_symbol) {
    impl->scope_bag.push_back(std::make_unique<Scope>(defining_symbol));
    return impl->scope_bag.back().get();
}

SourceContext const* SemaContext::get_source_context(Facet const* facet) const {
    while (facet) {
        if (auto* file = dyncast<SourceFileFacet const*>(facet)) {
            auto itr = impl->source_context_map.find(file);
            return itr != impl->source_context_map.end() ? itr->second :
                                                           nullptr;
        }
        facet = facet->parent();
    }
    return nullptr;
}

BuiltinTrait* SemaContext::get_type_trait() const {
    return impl->builtins.type_trait;
}

template <typename KeyType, typename T>
static T get_or_make(utl::hashmap<KeyType, T>& map, auto&& key, auto&& ctor) {
    auto itr = map.find(key);
    if (itr != map.end()) return itr->second;
    auto result = ctor();
    map.insert({ key, result });
    return result;
}

StructType* SemaContext::get_struct_specialization(
    StructDef* definition, std::span<Symbol* const> generic_args) {
    return get_or_make(impl->struct_specializations,
                       StructSpecKeyView{ definition, generic_args }, [&] {
        return make<StructType>(/* facet: */ nullptr, definition, generic_args);
    });
}

TraitInst* SemaContext::get_trait_specialization(
    TraitDef* definition, std::span<Symbol* const> generic_args) {
    return get_or_make(impl->trait_specializations,
                       TraitSpecKeyView{ definition, generic_args }, [&] {
        return make<TraitInst>(/* facet: */ nullptr, definition, generic_args);
    });
}

Symbol* SemaContext::add_symbol(csp::unique_ptr<Symbol> sym) {
    auto* s = sym.get();
    impl->symbol_bag.push_back(std::move(sym));
    return s;
}

void SemaContext::map_source_to_context(SourceFileFacet const* facet,
                                        SourceContext const* ctx) {
    auto [itr, result] = impl->source_context_map.insert({ facet, ctx });
    PRISM_ASSERT(result, "Failed to insert source file. Was it added twice?");
}
