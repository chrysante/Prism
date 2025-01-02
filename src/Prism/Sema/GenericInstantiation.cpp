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
};

struct MapContext {
    SemaContext& ctx;
    std::span<Symbol* const> genArgs;
    std::span<Symbol* const> genParams;

    ValueType* mapType(ValueType* type);
    GenStructTypeInst* cloneInstantiation(GenStructTypeInst& orig);
    Symbol* selectGenArg(Symbol* arg);
};

} // namespace prism

Symbol* MapContext::selectGenArg(Symbol* arg) {
    if (auto* typeInst = dyncast<GenStructTypeInst*>(arg))
        return mapType(typeInst);
    if (isa<GenericTypeParam>(arg))
        for (auto [newArg, param]: zip(genArgs, genParams))
            if (param == arg) return newArg;
    return arg;
}

GenStructTypeInst* MapContext::cloneInstantiation(GenStructTypeInst& orig) {
    auto newArgs = orig.genArguments() | transform(FN1(&, selectGenArg(_1))) |
                   ToSmallVector<>;
    auto* newInst = instantiateGeneric(ctx, *orig.typeTemplate(), newArgs);
    return cast<GenStructTypeInst*>(newInst);
}

ValueType* MapContext::mapType(ValueType* type) {
    if (auto* genType = dyncast<GenStructTypeInst*>(type))
        return cloneInstantiation(*genType);
    return type;
}

template <std::derived_from<ValueType> T>
static T const* mapType(SemaContext& ctx, T const* type,
                        std::span<Symbol* const> genArgs,
                        std::span<Symbol* const> genParams) {
    MapContext mapContext{ ctx, genArgs, genParams };
    auto* result = mapContext.mapType(const_cast<T*>(type));
    return cast<T const*>(result);
}

Symbol* GenInstContext::instantiate(GenStructType& templ) {
    auto [instantiation, isNew] =
        ctx.getGenericInst<GenStructTypeInst>(&templ, genArgs);
    if (!isNew) return instantiation;
    for (auto* trait: templ.baseTraits()) {
        PRISM_UNIMPLEMENTED();
    }
    for (auto* base: templ.baseClasses()) {
        auto* type = mapType(ctx, base->type(), genArgs, templ.genParams());
        auto* newBase = ctx.make<BaseClass>(base->facet(),
                                            instantiation->associatedScope(),
                                            type);
        instantiation->_bases.push_back(newBase);
    }
    for (auto* var: templ.memberVars()) {
        auto* type = mapType(ctx, var->type(), genArgs, templ.genParams());
        auto* newVar = ctx.make<MemberVar>(var->name(), var->facet(),
                                           instantiation->associatedScope(),
                                           type);
        instantiation->_memvars.push_back(newVar);
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
