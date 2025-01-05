#include "Prism/Sema/GenericInstantiation.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/ipp.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
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
    GenericInstantiation& inst;

    template <std::derived_from<Symbol> S>
    S* mapInst(S* symbol) {
        return mapInstantiation(ctx, symbol, inst.genArguments(),
                                inst.genTemplate()->genParams());
    }

    void instantiate(Symbol const&) { PRISM_UNREACHABLE(); }
    void instantiate(GenStructTypeInst& inst);
    void instantiate(GenTraitInst& inst);
};

} // namespace prism

namespace {

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

} // namespace

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
    return instantiateGenericNoFail(ctx, *inst.genTemplate(), newArgs);
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

void GenInstContext::instantiate(GenStructTypeInst& inst) {
    if (inst.isAnalyzed()) return;
    inst.setAnalyzed();
    for (auto* base: inst.genTemplate()->baseTraits()) {
        auto* trait = mapInst(base->trait());
        auto* newBase =
            ctx.make<BaseTrait>(base->facet(), inst.associatedScope(), trait);
        inst._baseTraits.push_back(newBase);
    }
    for (auto* base: inst.genTemplate()->baseClasses()) {
        auto* type = mapInst(base->type());
        auto* newBase =
            ctx.make<BaseClass>(base->facet(), inst.associatedScope(), type);
        inst._bases.push_back(newBase);
    }
    for (auto* var: inst.genTemplate()->memberVars()) {
        auto* type = mapInst(var->type());
        auto* newVar = ctx.make<MemberVar>(var->name(), var->facet(),
                                           inst.associatedScope(), type);
        inst._memvars.push_back(newVar);
    }
}

void GenInstContext::instantiate(GenTraitInst& inst) {
    if (inst.isAnalyzed()) return;
    inst.setAnalyzed();
    for (auto* base: inst.genTemplate()->baseTraits()) {
        auto* trait = mapInst(base->trait());
        auto* newBase =
            ctx.make<BaseTrait>(base->facet(), inst.associatedScope(), trait);
        inst._baseTraits.push_back(newBase);
    }
}

static bool validateArguments(SemaContext const& ctx, DiagnosticEmitter& DE,
                              GenericSymbol& gensym, Facet const* callFacet,
                              std::span<Symbol* const> args,
                              std::span<Facet const* const> argFacets) {
    utl::small_vector<Facet const*> backupFacets(args.size());
    if (argFacets.empty()) argFacets = backupFacets;
    PRISM_ASSERT(argFacets.size() == args.size());
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
        if (!typeParam->traitBound()) {
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
                                  std::span<Symbol* const> args) {
    auto* inst = instantiateGenericLazy(ctx, gensym, args);
    return completeInstantiation(ctx, DE, *inst, callFacet);
}

Symbol* prism::instantiateGenericLazy(SemaContext& ctx, GenericSymbol& genSym,
                                      std::span<Symbol* const> args) {
    // clang-format off
    return visit<Symbol*>(genSym,csp::overload{
        [](GenericSymbol const&) { PRISM_UNREACHABLE(); },
        [&](GenStructType& templ) {
            return ctx.getGenericInst<GenStructTypeInst>(&templ, args).first;
        },
        [&](GenTrait& templ) {
            return ctx.getGenericInst<GenTraitInst>(&templ, args).first;
        },
    }); // clang-format on
}

static std::span<Facet const* const> getArgFacets(Facet const* facet) {
    if (auto* call = dyncast<CallFacet const*>(facet))
        return call->arguments()->elems();
    return {};
}

static Symbol* doCompleteInst(SemaContext& ctx, DiagnosticEmitter& DE,
                              Symbol& symbol, GenericInstantiation& inst,
                              Facet const* facet) {
    if (!validateArguments(ctx, DE, *inst.genTemplate(), facet,
                           inst.genArguments(), getArgFacets(facet)))
        return nullptr;
    GenInstContext instContext{ ctx, inst };
    visit(symbol, FN1(&, instContext.instantiate(_1)));
    return &symbol;
}

Symbol* prism::completeInstantiation(SemaContext& ctx, DiagnosticEmitter& DE,
                                     Symbol& symbol, Facet const* facet) {
    return visit(symbol, [&]<typename S>(S& symbol) -> Symbol* {
        if constexpr (std::derived_from<S, GenericInstantiation>)
            return doCompleteInst(ctx, DE, symbol, symbol, facet);
        PRISM_UNREACHABLE();
    });
}

Symbol* prism::instantiateGenericNoFail(SemaContext& ctx, GenericSymbol& gen,
                                        std::span<Symbol* const> args) {
    auto DE = makeTrappingDiagnosticEmitter();
    return instantiateGeneric(ctx, *DE, gen, nullptr, args);
}
