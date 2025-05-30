#include "Prism/Sema/SemaContext.h"

#include <bit>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/hashtable.hpp>

#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/FuncSig.h"
#include "Prism/Sema/GenericMatching.h"
#include "Prism/Sema/Symbol.h"

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

template <typename Def, typename SubCtx>
struct GenInstKeyImpl {
    Def* generic;
    SubCtx sub_context;

    template <typename SubCtxArg = SubCtx&&>
    GenInstKeyImpl(Def* generic, SubCtxArg&& sub_context):
        generic(generic), sub_context(std::forward<SubCtxArg>(sub_context)) {}

    template <std::convertible_to<SubCtx> SubCtxRhs>
    GenInstKeyImpl(GenInstKeyImpl<Def, SubCtxRhs> const& rhs):
        generic(rhs.generic), sub_context(rhs.sub_context) {}

    template <typename SubCtxRhs>
    bool operator==(GenInstKeyImpl<Def, SubCtxRhs> const& rhs) const {
        return generic == rhs.generic && sub_context == rhs.sub_context;
        ;
    }
};

} // namespace

using StructInstKey = GenInstKeyImpl<StructDef, SubContext>;
using StructInstKeyView = GenInstKeyImpl<StructDef, SubContext const&>;
using TypeAliasInstKey = GenInstKeyImpl<TypeAliasDef, SubContext>;
using TypeAliasInstKeyView = GenInstKeyImpl<TypeAliasDef, SubContext const&>;
using TraitInstKey = GenInstKeyImpl<TraitDef, SubContext>;
using TraitInstKeyView = GenInstKeyImpl<TraitDef, SubContext const&>;
using FuncInstKey = GenInstKeyImpl<FunctionDef, SubContext>;
using FuncInstKeyView = GenInstKeyImpl<FunctionDef, SubContext const&>;

template <typename Def, typename SubCtx>
struct std::hash<GenInstKeyImpl<Def, SubCtx>> {
    size_t operator()(GenInstKeyImpl<Def, SubCtx> const& key) const {
        return utl::hash_combine(key.generic, key.sub_context.hash_value());
    }
};

namespace {

struct GenericParamKey {
    Symbol const* bound;
    size_t index;
    size_t nesting_depth;

    bool operator==(GenericParamKey const&) const = default;
};

} // namespace

template <>
struct std::hash<GenericParamKey> {
    size_t operator()(GenericParamKey const& key) const {
        return utl::hash_combine(key.bound, key.index, key.nesting_depth);
    }
};

struct SemaContext::Impl {
    Module* mod = nullptr;
    Library* core_libary = nullptr;
    std::vector<csp::unique_ptr<Symbol>> symbol_bag;
    std::vector<std::unique_ptr<Scope>> scope_bag;
    utl::hashmap<StructInstKey, StructInst*> struct_instantiations;
    utl::hashmap<TypeAliasInstKey, TypeAliasInst*> type_alias_instantiations;
    utl::hashmap<TraitInstKey, TraitInst*> trait_instantiations;
    utl::hashmap<Trait*, TraitThisType*> trait_this_types;
    utl::hashmap<FuncInstKey, FunctionInst*> function_instantiations;
    utl::hashmap<FuncSig, FunctionType*> function_types;
    utl::hashmap<GenericParamKey, Symbol*> generic_parameters;
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

template <typename KeyType, typename T, typename KeyTypeU = KeyType>
static T get_or_make(utl::hashmap<KeyType, T>& map, KeyTypeU&& key,
                     auto&& ctor) {
    auto itr = map.find(key);
    if (itr != map.end()) return itr->second;
    auto result = ctor();
    map.insert({ key, result });
    return result;
}

StructInst* SemaContext::get_struct_instantiation(SubContext const& sub_context,
                                                  StructDef* definition) {
    return get_or_make(impl->struct_instantiations,
                       StructInstKeyView{ definition, sub_context }, [&] {
        return make<StructInst>(definition, sub_context);
    });
}

namespace {

struct SubstitutionMatcher {
    SemaContext& ctx;
    SubContext const& sub_context;

    Symbol* null_fallback() const { return nullptr; }

    Symbol* structural(StructInst& struct_inst) const {
        SubContext inner_ctx = struct_inst.sub_context();
        for (auto& arg: inner_ctx.flat_view())
            arg = match_generic(*this, arg);
        return ctx.get_struct_instantiation(inner_ctx,
                                            struct_inst.definition());
    }

    Symbol* base_case(Symbol& input) const {
        if (auto* gen_param = as_gen_param_base(&input))
            return sub_context.resolve(*gen_param);
        return canonicalize(&input);
    }
};

} // namespace

static Symbol* substitute_symbol(SemaContext& ctx,
                                 SubContext const& sub_context, Symbol* input) {
    return match_generic(SubstitutionMatcher{ ctx, sub_context }, input);
}

TypeAliasInst* SemaContext::get_type_alias_instantiation(
    SubContext const& sub_context, TypeAliasDef* definition) {
    return get_or_make(impl->type_alias_instantiations,
                       TypeAliasInstKeyView{ definition, sub_context }, [&] {
        auto* aliased =
            substitute_symbol(*this, sub_context, definition->aliased());
        return make<TypeAliasInst>(definition, sub_context,
                                   cast<Type*>(aliased));
    });
}

TraitInst* SemaContext::get_trait_instantiation(SubContext const& sub_context,
                                                TraitDef* definition) {
    return get_or_make(impl->trait_instantiations,
                       TraitInstKeyView{ definition, sub_context }, [&] {
        return make<TraitInst>(definition, sub_context);
    });
}

TraitThisType* SemaContext::get_trait_this_type(Trait* trait) {
    return get_or_make(impl->trait_this_types, trait,
                       [&] { return make<TraitThisType>(trait); });
}

static FuncSig compute_signature(SemaContext& ctx,
                                 SubContext const& sub_context,
                                 FunctionDef const& definition) {
    auto sub_type = [&](Type const* type) {
        auto* mut_type = const_cast<Type*>(type);
        return cast<Type const*>(substitute_symbol(ctx, sub_context, mut_type));
    };
    auto to_func_arg_spec = [&](FunctionArgument const* param) {
        if (!param) return FuncArgSpec();
        return FuncArgSpec(param->passing_convention(),
                           sub_type(param->type()));
    };
    auto param_types = definition.arguments() | transform(to_func_arg_spec) |
                       ToSmallVector<>;
    auto* return_type = sub_type(definition.return_type());
    return FuncSig(param_types, return_type);
}

FunctionInst* SemaContext::get_function_instantiation(
    SubContext const& sub_context, FunctionDef* definition) {
    return get_or_make(impl->function_instantiations,
                       FuncInstKeyView{ definition, sub_context }, [&] {
        auto signature = compute_signature(*this, sub_context, *definition);
        auto* type = get_function_type(signature);
        return make<FunctionInst>(definition, sub_context, type);
    });
}

FunctionType const* SemaContext::get_function_type(FuncSig const& signature) {
    return get_or_make(impl->function_types, signature, [&] {
        return make<FunctionType>(impl->mod->scope(), signature);
    });
}

GenTypeParam* SemaContext::get_gen_type_param(
    Facet const* facet, Scope* parent_scope, std::string name,
    Trait const* trait_bound, size_t index, size_t nesting_depth) {
    auto* sym = get_or_make(impl->generic_parameters,
                            { trait_bound, index, nesting_depth }, [&] {
        return make<GenTypeParam>(trait_bound, index, nesting_depth);
    });
    parent_scope->add_symbol(*sym, name, facet,
                             /* participate_in_name_lookup: */ true);
    return cast<GenTypeParam*>(sym);
}

GenValueParam* SemaContext::get_gen_value_param(Facet const* facet,
                                                Scope* parent_scope,
                                                std::string name,
                                                Type const* type, size_t index,
                                                size_t nesting_depth) {
    auto* sym = get_or_make(impl->generic_parameters,
                            { type, index, nesting_depth }, [&] {
        return make<GenValueParam>(type, index, nesting_depth);
    });
    parent_scope->add_symbol(*sym, name, facet,
                             /* participate_in_name_lookup: */ true);
    return cast<GenValueParam*>(sym);
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
