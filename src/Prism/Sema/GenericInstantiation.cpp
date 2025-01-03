#include "Prism/Sema/GenericInstantiation.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::transform;
using ranges::views::zip;

namespace prism {

struct GenInstContext {
    SemaContext& ctx;
    std::span<Symbol* const> genArgs;

    Symbol* instantiate(GenericSymbol const&) { PRISM_UNREACHABLE(); }
    Symbol* instantiate(GenStructType& templ);
    Symbol* instantiate(GenTrait& templ);
};

struct MapContext {
    SemaContext& ctx;
    std::span<Symbol* const> genArgs;
    std::span<Symbol* const> genParams;

    Symbol* mapSymbol(Symbol* sym);
    Symbol* doMapSymbol(Symbol& sym) { return &sym; }
    Symbol* doMapSymbol(GenStructTypeInst& inst);
    Symbol* doMapSymbol(GenTraitInst& inst);
    Symbol* selectGenArg(Symbol* arg);
};

} // namespace prism

Symbol* MapContext::selectGenArg(Symbol* arg) {
    if (auto* typeInst = dyncast<GenStructTypeInst*>(arg))
        return mapSymbol(typeInst);
    if (isa<GenericTypeParam>(arg))
        for (auto [newArg, param]: zip(genArgs, genParams))
            if (param == arg) return newArg;
    return arg;
}

Symbol* MapContext::doMapSymbol(GenStructTypeInst& inst) {
    auto newArgs = inst.genArguments() | transform(FN1(&, selectGenArg(_1))) |
                   ToSmallVector<>;
    return instantiateGeneric(ctx, *inst.typeTemplate(), newArgs);
}

Symbol* MapContext::doMapSymbol(GenTraitInst& inst) {
    auto newArgs = inst.genArguments() | transform(FN1(&, selectGenArg(_1))) |
                   ToSmallVector<>;
    return instantiateGeneric(ctx, *inst.genTemplate(), newArgs);
}

Symbol* MapContext::mapSymbol(Symbol* sym) {
    PRISM_EXPECT(sym);
    return visit(*sym, FN1(&, doMapSymbol(_1)));
}

template <std::derived_from<Symbol> T>
static T* mapSymbol(SemaContext& ctx, T* sym, std::span<Symbol* const> genArgs,
                    std::span<Symbol* const> genParams) {
    MapContext mapContext{ ctx, genArgs, genParams };
    auto* result = mapContext.mapSymbol(sym);
    return cast<T*>(result);
}

Symbol* GenInstContext::instantiate(GenStructType& templ) {
    auto [instantiation, isNew] =
        ctx.getGenericInst<GenStructTypeInst>(&templ, genArgs);
    if (!isNew) return instantiation;
    for (auto* base: templ.baseTraits()) {
        auto* trait = mapSymbol(ctx, base->trait(), genArgs, templ.genParams());
        auto* newBase = ctx.make<BaseTrait>(base->facet(),
                                            instantiation->associatedScope(),
                                            trait);
        instantiation->_baseTraits.push_back(newBase);
    }
    for (auto* base: templ.baseClasses()) {
        auto* type = mapSymbol(ctx, base->type(), genArgs, templ.genParams());
        auto* newBase = ctx.make<BaseClass>(base->facet(),
                                            instantiation->associatedScope(),
                                            type);
        instantiation->_bases.push_back(newBase);
    }
    for (auto* var: templ.memberVars()) {
        auto* type = mapSymbol(ctx, var->type(), genArgs, templ.genParams());
        auto* newVar = ctx.make<MemberVar>(var->name(), var->facet(),
                                           instantiation->associatedScope(),
                                           type);
        instantiation->_memvars.push_back(newVar);
    }
    return instantiation;
}

Symbol* GenInstContext::instantiate(GenTrait& templ) {
    auto [instantiation, isNew] =
        ctx.getGenericInst<GenTraitInst>(&templ, genArgs);
    if (!isNew) return instantiation;
    for (auto* base: templ.baseTraits()) {
        auto* trait = mapSymbol(ctx, base->trait(), genArgs, templ.genParams());
        auto* newBase = ctx.make<BaseTrait>(base->facet(),
                                            instantiation->associatedScope(),
                                            trait);
        instantiation->_baseTraits.push_back(newBase);
    }
    return instantiation;
}

static bool conformsTo(ValueType const&, Trait const& trait) {
    if (trait.name() == "type") // Ugh, how to we fix this?!
        return true;            // All types conform to the `type` trait
    return false;               // For now
}

static bool validateArguments(GenericSymbol& gensym,
                              std::span<Symbol* const> args) {
    auto params = gensym.genParams();
    if (args.size() != params.size()) PRISM_UNIMPLEMENTED();
    for (auto [arg, param]: zip(args, params)) {
        auto* typeParam = cast<GenericTypeParam const*>(param);
        auto* typeArg = dyncast<ValueType const*>(arg);
        if (!typeArg || !conformsTo(*typeArg, *typeParam->trait()))
            PRISM_UNIMPLEMENTED();
    }
    return true;
}

Symbol* prism::instantiateGeneric(SemaContext& ctx, GenericSymbol& gensym,
                                  std::span<Symbol* const> args) {
    if (!validateArguments(gensym, args)) return nullptr;
    GenInstContext instContext{ ctx, args };
    return visit(gensym, FN1(&, instContext.instantiate(_1)));
}
