#include "Prism/Sema/GenericInstantiation.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/ConformanceAnalysis.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;
using ranges::views::transform;
using ranges::views::zip;

namespace prism {

struct GenInstContext {
    SemaContext& ctx;
    std::span<Symbol* const> genArgs;
    std::span<Symbol* const> genParams;

    template <std::derived_from<Symbol> S>
    S* mapInst(S* symbol) {
        return mapInstantiation(ctx, symbol, genArgs, genParams);
    }

    Symbol* instantiate(GenericSymbol const&) { PRISM_UNREACHABLE(); }
    Symbol* instantiate(GenStructType& templ);
    Symbol* instantiate(GenTrait& templ);
};

struct MapContext {
    SemaContext& ctx;
    std::span<Symbol* const> genArgs;
    std::span<Symbol* const> genParams;

    Symbol* mapSymbol(Symbol* sym);
    template <typename T>
    T* mapSymbol(Symbol* sym);
    Symbol* doMapSymbol(Symbol& sym) { return &sym; }
    Symbol* doMapSymbol(Function& func);
    Symbol* doMapSymbol(FuncParam& param);
    Symbol* doMapSymbol(GenStructTypeInst& inst);
    Symbol* doMapSymbol(GenTraitInst& inst);
    Symbol* selectGenArg(Symbol* arg);
    template <typename T>
    T* selectGenArg(Symbol* arg);
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

template <typename T>
T* MapContext::selectGenArg(Symbol* arg) {
    auto* result = selectGenArg(arg);
    return cast<T*>(result);
}

Symbol* MapContext::doMapSymbol(Function& func) {
    auto newParams = func.params() |
                     transform(FN1(&, mapSymbol<FuncParam>(_1))) |
                     ToSmallVector<>;
    auto newRet = selectGenArg<Type>(const_cast<Type*>(func.retType()));
    if (ranges::equal(newParams, func.params()) && newRet == func.retType())
        return &func;
    return ctx.make<Function>(func.name(), func.facet(), func.parentScope(),
                              std::move(newParams), newRet);
}

Symbol* MapContext::doMapSymbol(FuncParam& param) {
    auto* newType = selectGenArg<Type>(const_cast<Type*>(param.type()));
    if (newType == param.type()) return &param;
    return ctx.make<FuncParam>(param.name(), param.facet(), newType,
                               param.options());
}

Symbol* MapContext::doMapSymbol(GenStructTypeInst& inst) {
    auto newArgs = inst.genArguments() | transform(FN1(&, selectGenArg(_1))) |
                   ToSmallVector<>;
    return instantiateGenericNoFail(ctx, *inst.typeTemplate(), newArgs);
}

Symbol* MapContext::doMapSymbol(GenTraitInst& inst) {
    auto newArgs = inst.genArguments() | transform(FN1(&, selectGenArg(_1))) |
                   ToSmallVector<>;
    return instantiateGenericNoFail(ctx, *inst.genTemplate(), newArgs);
}

Symbol* MapContext::mapSymbol(Symbol* sym) {
    PRISM_EXPECT(sym);
    PRISM_EXPECT(genArgs.size() == genParams.size());
    return visit(*sym, FN1(&, doMapSymbol(_1)));
}

template <typename T>
T* MapContext::mapSymbol(Symbol* sym) {
    auto* result = mapSymbol(sym);
    return cast<T*>(result);
}

Symbol* prism::mapInstantiation(SemaContext& ctx, Symbol* symbol,
                                std::span<Symbol* const> genArgs,
                                std::span<Symbol* const> genParams) {
    return MapContext{ ctx, genArgs, genParams }.mapSymbol(symbol);
}

Symbol* GenInstContext::instantiate(GenStructType& templ) {
    auto [instantiation, isNew] =
        ctx.getGenericInst<GenStructTypeInst>(&templ, genArgs);
    if (!isNew) return instantiation;
    for (auto* base: templ.baseTraits()) {
        auto* trait = mapInst(base->trait());
        auto* newBase = ctx.make<BaseTrait>(base->facet(),
                                            instantiation->associatedScope(),
                                            trait);
        instantiation->_baseTraits.push_back(newBase);
    }
    for (auto* base: templ.baseClasses()) {
        auto* type = mapInst(base->type());
        auto* newBase = ctx.make<BaseClass>(base->facet(),
                                            instantiation->associatedScope(),
                                            type);
        instantiation->_bases.push_back(newBase);
    }
    for (auto* var: templ.memberVars()) {
        auto* type = mapInst(var->type());
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
        auto* trait = mapInst(base->trait());
        auto* newBase = ctx.make<BaseTrait>(base->facet(),
                                            instantiation->associatedScope(),
                                            trait);
        instantiation->_baseTraits.push_back(newBase);
    }
    return instantiation;
}

static bool validateArguments(SemaContext const& ctx, DiagnosticEmitter& DE,
                              GenericSymbol& gensym, Facet const* callFacet,
                              std::span<Symbol* const> args,
                              std::span<Facet const* const> argFacets) {
    auto params = gensym.genParams();
    if (args.size() != params.size()) {
        DE.emit<InvalidNumOfGenArgs>(ctx.getSourceContext(callFacet), callFacet,
                                     &gensym, args.size());
        return false;
    }
    bool result = true;
    for (auto [param, arg, facet]: zip(params, args, argFacets)) {
        auto* typeParam = cast<GenericTypeParam const*>(param);
        auto* typeArg = dyncast<ValueType const*>(arg);
        if (!typeArg) {
            DE.emit<BadSymRef>(ctx.getSourceContext(facet), facet, arg,
                               SymbolType::ValueType);
            result = false;
            continue;
        }
        if (!conformsTo(*typeArg, *typeParam->traitBound())) {
            DE.emit<BadGenTypeArg>(ctx.getSourceContext(facet), facet, typeArg,
                                   typeParam->traitBound());
            result = false;
            continue;
        }
    }
    return result;
}

Symbol* prism::instantiateGeneric(SemaContext& ctx, DiagnosticEmitter& DE,
                                  GenericSymbol& gensym, Facet const* callFacet,
                                  std::span<Symbol* const> args,
                                  std::span<Facet const* const> argFacets) {
    if (!validateArguments(ctx, DE, gensym, callFacet, args, argFacets))
        return nullptr;
    GenInstContext instContext{ ctx, args, gensym.genParams() };
    return visit(gensym, FN1(&, instContext.instantiate(_1)));
}

Symbol* prism::instantiateGenericNoFail(SemaContext& ctx, GenericSymbol& gen,
                                        std::span<Symbol* const> args) {
    auto DE = makeTrappingDiagnosticEmitter();
    auto facets = args | transform(FN1((Facet const*)nullptr)) |
                  ToSmallVector<>;
    auto* sym = instantiateGeneric(ctx, *DE, gen, nullptr, args, facets);
    return sym;
}
