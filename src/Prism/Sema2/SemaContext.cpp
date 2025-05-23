#include "Prism/Sema2/SemaContext.h"

#include <bit>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>

#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/FuncSig.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::transform;

namespace {

struct Builtins {
    BuiltinTrait* type_trait = nullptr;
};

template <typename Def, typename ArgsContainer>
struct GenInstKeyImpl {
    Def* generic;
    ArgsContainer args;

    template <typename Cont = ArgsContainer&&>
    GenInstKeyImpl(Def* generic, Cont&& args):
        generic(generic), args(std::forward<Cont>(args)) {}

    template <std::convertible_to<ArgsContainer> ArgsContainerRhs>
    GenInstKeyImpl(GenInstKeyImpl<Def, ArgsContainerRhs> const& rhs):
        generic(rhs.generic), args(rhs.args) {}

    GenInstKeyImpl(GenInstKeyImpl<Def, std::span<Symbol const* const>> rhs):
        generic(rhs.generic), args(rhs.args | ToSmallVector<>) {}

    template <typename ArgsContainerRhs>
    bool operator==(GenInstKeyImpl<Def, ArgsContainerRhs> const& rhs) const {
        return generic == rhs.generic && ranges::equal(args, rhs.args);
    }
};

} // namespace

using StructInstKey =
    GenInstKeyImpl<StructDef, utl::small_vector<Symbol const*>>;
using StructInstKeyView =
    GenInstKeyImpl<StructDef, std::span<Symbol const* const>>;
using TraitInstKey = GenInstKeyImpl<TraitDef, utl::small_vector<Symbol const*>>;
using TraitInstKeyView =
    GenInstKeyImpl<TraitDef, std::span<Symbol const* const>>;
using FuncInstKey =
    GenInstKeyImpl<FunctionDef, utl::small_vector<Symbol const*>>;
using FuncInstKeyView =
    GenInstKeyImpl<FunctionDef, std::span<Symbol const* const>>;

template <typename Def, typename ArgsContainer>
struct std::hash<GenInstKeyImpl<Def, ArgsContainer>> {
    size_t operator()(GenInstKeyImpl<Def, ArgsContainer> const& key) const {
        size_t seed = 0;
        utl::hash_combine(seed, key.generic);
        ranges::for_each(key.args, FN1(&, utl::hash_combine(seed, _1)));
        return seed;
    }
};

struct SemaContext::Impl {
    Module* mod = nullptr;
    std::vector<csp::unique_ptr<Symbol>> symbol_bag;
    std::vector<std::unique_ptr<Scope>> scope_bag;
    utl::hashmap<SourceFileFacet const*, SourceContext const*>
        source_context_map;
    utl::hashmap<StructInstKey, StructInst*> struct_instantiations;
    utl::hashmap<TraitInstKey, TraitInst*> trait_instantiations;
    utl::hashmap<FuncInstKey, FunctionInst*> function_instantiations;
    utl::hashmap<FuncSig, FunctionType*> function_types;
    Builtins builtins;
};

SemaContext::SemaContext() = default;

SemaContext::~SemaContext() = default;

Module* SemaContext::make_module() {
    PRISM_ASSERT(impl->mod == nullptr, "make_module() has been called before");
    auto* mod = impl->mod = make<Module>();
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

template <typename KeyType, typename T>
static T get_or_make(utl::hashmap<KeyType, T>& map, auto&& key, auto&& ctor) {
    auto itr = map.find(key);
    if (itr != map.end()) return itr->second;
    auto result = ctor();
    map.insert({ key, result });
    return result;
}

StructInst* SemaContext::get_struct_instantiation(
    StructDef* definition, std::span<Symbol* const> generic_args) {
    return get_or_make(impl->struct_instantiations,
                       StructInstKeyView{ definition, generic_args }, [&] {
        return make<StructInst>(/* facet: */ nullptr, definition, generic_args);
    });
}

TraitInst* SemaContext::get_trait_instantiation(
    TraitDef* definition, std::span<Symbol* const> generic_args) {
    return get_or_make(impl->trait_instantiations,
                       TraitInstKeyView{ definition, generic_args }, [&] {
        return make<TraitInst>(/* facet: */ nullptr, definition, generic_args);
    });
}

static FuncSig compute_signature(FunctionDef const& definition,
                                 std::span<Symbol* const> generic_args) {
    // The assertions here are temporary until we implement generic substitution
    PRISM_ASSERT(generic_args.empty());
    PRISM_ASSERT(definition.generic_params().empty());
    auto args = definition.arguments() |
                transform([](FunctionArgument const* arg) {
        return FuncArgSpec(arg->passing_convention(), arg->type());
    }) | ToSmallVector<>;
    return FuncSig(args, definition.return_type());
}

FunctionInst* SemaContext::get_function_instantiation(
    FunctionDef* definition, std::span<Symbol* const> generic_args) {
    return get_or_make(impl->function_instantiations,
                       FuncInstKeyView{ definition, generic_args }, [&] {
        auto signature = compute_signature(*definition, generic_args);
        auto* type = get_function_type(signature);
        return make<FunctionInst>(/* facet: */ nullptr, definition, type,
                                  generic_args);
    });
}

FunctionType const* SemaContext::get_function_type(FuncSig const& signature) {
    return get_or_make(impl->function_types, signature, [&] {
        return make<FunctionType>(impl->mod->scope(), signature);
    });
}

BuiltinTrait* SemaContext::get_type_trait() const {
    return impl->builtins.type_trait;
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
