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
    BuiltinType* void_type = nullptr;
    BuiltinType* bool_type = nullptr;
    BuiltinType* byte_type = nullptr;
    BuiltinType* i8_type = nullptr;
    BuiltinType* i16_type = nullptr;
    BuiltinType* i32_type = nullptr;
    BuiltinType* i64_type = nullptr;
    BuiltinType* u8_type = nullptr;
    BuiltinType* u16_type = nullptr;
    BuiltinType* u32_type = nullptr;
    BuiltinType* u64_type = nullptr;
    BuiltinType* f32_type = nullptr;
    BuiltinType* f64_type = nullptr;
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
    Library* core_libary = nullptr;
    std::vector<csp::unique_ptr<Symbol>> symbol_bag;
    std::vector<std::unique_ptr<Scope>> scope_bag;
    utl::hashmap<SourceFileFacet const*, SourceContext const*>
        source_context_map;
    utl::hashmap<StructInstKey, StructInst*> struct_instantiations;
    utl::hashmap<TraitInstKey, TraitInst*> trait_instantiations;
    utl::hashmap<FuncInstKey, FunctionInst*> function_instantiations;
    utl::hashmap<FuncSig, FunctionType*> function_types;
    utl::hashmap<Facet const*, IntLiteral*> int_literals;
    Builtins builtins;
};

SemaContext::SemaContext() = default;

SemaContext::~SemaContext() = default;

static Builtins make_builtins(SemaContext& ctx, Scope* parent_scope) {
    return {
        .type_trait =
            ctx.make<BuiltinTrait>(parent_scope, "type", ScopeArg::make(ctx)),
        .void_type = ctx.make<BuiltinType>(parent_scope, "void",
                                           ScopeArg::make(ctx), TypeLayout(0)),
        .bool_type = ctx.make<BuiltinType>(parent_scope, "bool",
                                           ScopeArg::make(ctx), TypeLayout(1)),
        .byte_type = ctx.make<BuiltinType>(parent_scope, "byte",
                                           ScopeArg::make(ctx), TypeLayout(1)),
        .i8_type = ctx.make<BuiltinType>(parent_scope, "i8",
                                         ScopeArg::make(ctx), TypeLayout(1)),
        .i16_type = ctx.make<BuiltinType>(parent_scope, "i16",
                                          ScopeArg::make(ctx), TypeLayout(2)),
        .i32_type = ctx.make<BuiltinType>(parent_scope, "i32",
                                          ScopeArg::make(ctx), TypeLayout(4)),
        .i64_type = ctx.make<BuiltinType>(parent_scope, "i64",
                                          ScopeArg::make(ctx), TypeLayout(8)),
        .u8_type = ctx.make<BuiltinType>(parent_scope, "u8",
                                         ScopeArg::make(ctx), TypeLayout(1)),
        .u16_type = ctx.make<BuiltinType>(parent_scope, "u16",
                                          ScopeArg::make(ctx), TypeLayout(2)),
        .u32_type = ctx.make<BuiltinType>(parent_scope, "u32",
                                          ScopeArg::make(ctx), TypeLayout(4)),
        .u64_type = ctx.make<BuiltinType>(parent_scope, "u64",
                                          ScopeArg::make(ctx), TypeLayout(8)),
        .f32_type = ctx.make<BuiltinType>(parent_scope, "f32",
                                          ScopeArg::make(ctx), TypeLayout(4)),
        .f64_type = ctx.make<BuiltinType>(parent_scope, "f64",
                                          ScopeArg::make(ctx), TypeLayout(8))
    };
}

static Library* make_core_library(SemaContext& ctx, Scope* parent_scope) {
    auto* lib = ctx.make<Library>(parent_scope, "core");
    return lib;
}

Module* SemaContext::make_module() {
    PRISM_ASSERT(impl->mod == nullptr, "make_module() has been called before");
    auto* mod = impl->mod = make<Module>();
    impl->builtins = make_builtins(*this, mod->scope());
    impl->core_libary = make_core_library(*this, mod->scope());
    return mod;
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

static BuiltinType const* get_int_literal_type(SemaContext const& ctx,
                                               size_t bitwidth,
                                               bool is_signed) {
    switch (bitwidth) {
    case 8:
        return is_signed ? ctx.get_i8_type() : ctx.get_u8_type();
    case 16:
        return is_signed ? ctx.get_i16_type() : ctx.get_u16_type();
    case 32:
        return is_signed ? ctx.get_i32_type() : ctx.get_u32_type();
    case 64:
        return is_signed ? ctx.get_i64_type() : ctx.get_u64_type();
    default:
        PRISM_UNREACHABLE();
    }
}

IntLiteral* SemaContext::get_int_literal(Facet const* facet, APInt value,
                                         bool is_signed) {
    return get_or_make(impl->int_literals, facet, [&] {
        size_t bitwidth = value.bitwidth();
        auto* type = get_int_literal_type(*this, bitwidth, is_signed);
        return make<IntLiteral>(facet, std::move(value), type);
    });
}

BuiltinTrait* SemaContext::get_type_trait() const {
    return impl->builtins.type_trait;
}
BuiltinType* SemaContext::get_void_type() const {
    return impl->builtins.void_type;
}
BuiltinType* SemaContext::get_bool_type() const {
    return impl->builtins.bool_type;
}
BuiltinType* SemaContext::get_byte_type() const {
    return impl->builtins.byte_type;
}
BuiltinType* SemaContext::get_i8_type() const { return impl->builtins.i8_type; }
BuiltinType* SemaContext::get_i16_type() const {
    return impl->builtins.i16_type;
}
BuiltinType* SemaContext::get_i32_type() const {
    return impl->builtins.i32_type;
}
BuiltinType* SemaContext::get_i64_type() const {
    return impl->builtins.i64_type;
}
BuiltinType* SemaContext::get_u8_type() const { return impl->builtins.u8_type; }
BuiltinType* SemaContext::get_u16_type() const {
    return impl->builtins.u16_type;
}
BuiltinType* SemaContext::get_u32_type() const {
    return impl->builtins.u32_type;
}
BuiltinType* SemaContext::get_u64_type() const {
    return impl->builtins.u64_type;
}
BuiltinType* SemaContext::get_f32_type() const {
    return impl->builtins.f32_type;
}
BuiltinType* SemaContext::get_f64_type() const {
    return impl->builtins.f64_type;
}

Symbol* SemaContext::add_symbol(csp::unique_ptr<Symbol> sym) {
    auto* s = sym.get();
    impl->symbol_bag.push_back(std::move(sym));
    return s;
}

Scope* SemaContext::add_scope(std::unique_ptr<Scope> scope) {
    auto* s = scope.get();
    impl->scope_bag.push_back(std::move(scope));
    return s;
}

void SemaContext::map_source_to_context(SourceFileFacet const* facet,
                                        SourceContext const* ctx) {
    auto [itr, result] = impl->source_context_map.insert({ facet, ctx });
    PRISM_ASSERT(result, "Failed to insert source file. Was it added twice?");
}
