#include "Prism/Sema/ConformanceAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/DiagnosticHandler.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/GenericInstantiation.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::concat;
using ranges::views::reverse;
using ranges::views::transform;
using ranges::views::zip;

static csp::unique_ptr<Obligation> clone(Obligation const& obl) {
    return visit<csp::unique_ptr<Obligation>>(obl, []<class T>(T const& obl) {
        return csp::make_unique<T>(obl);
    });
}

template <std::derived_from<Obligation> O>
static csp::unique_ptr<O> clone(O const& obl) {
    auto c = clone(static_cast<Obligation const&>(obl));
    return csp::unique_ptr<O>(cast<O*>(c.release()));
}

static csp::unique_ptr<Obligation> cloneInstantiate(
    SemaContext& ctx, Obligation const& obl, std::span<Symbol* const> genArgs,
    std::span<Symbol* const> genParams) {
    auto result = clone(obl);
    auto* newSym = mapInstantiation(ctx, result->symbol(), genArgs, genParams);
    result->setSymbol(newSym);
    return result;
}

template <std::derived_from<Obligation> O>
static csp::unique_ptr<O> cloneInstantiate(SemaContext& ctx, O const& obl,
                                           std::span<Symbol* const> genArgs,
                                           std::span<Symbol* const> genParams) {
    auto c = cloneInstantiate(ctx, static_cast<Obligation const&>(obl), genArgs,
                              genParams);
    return csp::unique_ptr<O>(cast<O*>(c.release()));
}

namespace prism {

struct ConfAnaContext: AnalysisBase {
    void analyzeObligation(Symbol* sym, InterfaceLike& interface);
    void doAnalyzeObligation(Symbol const&, InterfaceLike&) {}
    void doAnalyzeObligation(Function& func, InterfaceLike& interface);
    void analyzeObligations(InterfaceLike& interface, Scope* scope);
    void analyzeConformance(Symbol* sym, InterfaceLike& interface);
    void doAnalyzeConformance(Symbol const&, InterfaceLike&) {}
    void doAnalyzeConformance(FunctionImpl& func, InterfaceLike& interface);
    void analyzeConformances(InterfaceLike& interface, Scope* scope);
    void inheritObligations(InterfaceLike const& base, InterfaceLike& derived);
    void copyInstantiate(InterfaceLike& from, InterfaceLike& to,
                         std::span<Symbol* const> genArgs,
                         std::span<Symbol* const> genParams);
    void analyze(Symbol const&) {}
    void doAnalyze(TraitImplInterface& interface);
    void analyze(TraitDef& trait);
    void analyze(GenTraitInst& trait);
    void analyze(GenTrait& trait);
    void analyze(TraitImplDef& impl);
    void analyze(GenTraitImplInst& impl);
    void analyze(GenTraitImpl& impl);
    void analyze(CompositeType& type);
    void analyze(BaseTrait& base);
    void analyze(BaseClass& base);
};

} // namespace prism

void ConfAnaContext::analyzeObligation(Symbol* sym, InterfaceLike& interface) {
    return visit(*sym, FN1(&, doAnalyzeObligation(_1, interface)));
}

void ConfAnaContext::doAnalyzeObligation(Function& func,
                                         InterfaceLike& interface) {
    if (func.params().empty() || !func.params().front()->isThis()) return;
    auto* owner = func.parentScope()->assocSymbol();
    if (!isa<TraitDef>(owner) && !isa<GenTrait>(owner)) return;
    auto obl = csp::make_unique<FuncObligation>(&func, owner);
    interface.addObligation(std::move(obl), SpecAddMode::Define);
}

void ConfAnaContext::analyzeObligations(InterfaceLike& interface,
                                        Scope* scope) {
    ranges::for_each(scope->symbols(),
                     FN1(&, analyzeObligation(_1, interface)));
}

void ConfAnaContext::analyzeConformance(Symbol* sym, InterfaceLike& interface) {
    if (!sym) return;
    visit(*sym, FN1(&, doAnalyzeConformance(_1, interface)));
}

void ConfAnaContext::doAnalyzeConformance(FunctionImpl& func,
                                          InterfaceLike& interface) {
    if (func.params().empty() || !func.params().front()->isThis()) return;
    auto matches = interface.matchObligation(func.name(), func.signature());
    if (matches.empty()) return;
    if (matches.size() == 1) {
        matches.front()->addConformance(&func, SpecAddMode::Define);
        return;
    }
    diagHandler
        .push<AmbiguousConformance>(sourceContext, func.facet(), &func,
                                    matches | ToSmallVector<Obligation const*>);
}

void ConfAnaContext::analyzeConformances(InterfaceLike& interface,
                                         Scope* scope) {
    for (auto* sym: scope->symbols())
        analyzeConformance(sym, interface);
}

void ConfAnaContext::inheritObligations(InterfaceLike const& base,
                                        InterfaceLike& derived) {
    for (auto& [key, list]: base.obligations())
        for (auto* obl: list)
            derived.addObligation(clone(*obl), SpecAddMode::Inherit);
}

void ConfAnaContext::copyInstantiate(InterfaceLike& from, InterfaceLike& to,
                                     std::span<Symbol* const> genArgs,
                                     std::span<Symbol* const> genParams) {
    for (auto& [key, list]: from.obligations())
        for (auto* obl: list)
            to.addObligation(cloneInstantiate(ctx, *obl, genArgs, genParams),
                             SpecAddMode::Inherit);
}

void ConfAnaContext::analyze(TraitDef& trait) {
    analyzeObligations(trait, trait.associatedScope());
    analyzeConformances(trait, trait.associatedScope());
}

void ConfAnaContext::analyze(GenTraitInst& trait) {
    copyInstantiate(*trait.genTemplate(), trait, trait.genArguments(),
                    trait.genTemplate()->genParams());
}

void ConfAnaContext::analyze(GenTrait& trait) {
    analyzeObligations(trait, trait.associatedScope());
    analyzeConformances(trait, trait.associatedScope());
}

static InterfaceLike& getInterfaceLike(Symbol& sym) {
    return visit(sym, []<typename T>(T& sym) -> InterfaceLike& {
        if constexpr (std::derived_from<T, InterfaceLike>)
            return sym;
        else
            PRISM_UNREACHABLE();
    });
}

void ConfAnaContext::doAnalyze(TraitImplInterface& interface) {
    auto* trait = interface.trait();
    auto* conf = interface.conformingType();
    if (!trait || !conf) return;
    auto* existing = [&]() -> Symbol const* {
        if (auto* ex = conf->findTraitImpl(trait)) return &ex->traitImpl();
        if (auto* compType = dyncast<CompositeType*>(conf)) {
            auto baseTraits = compType->baseTraits();
            auto itr = ranges::find(baseTraits, trait, FN1(_1->trait()));
            return itr != baseTraits.end() ? *itr : nullptr;
        }
        return nullptr;
    }();
    auto& impl = interface.traitImpl();
    if (existing) {
        diagHandler.push<DuplicateTraitImpl>(sourceContext, impl.facet(), &impl,
                                             existing);
        return;
    }
    inheritObligations(*trait, interface);
    analyzeConformances(interface, impl.associatedScope());
    if (!interface.isComplete())
        diagHandler.push<IncompleteImpl>(sourceContext, impl.facet(), &impl,
                                         interface);
    // clang-format off
    visit(impl, csp::overload{
        [&](TraitImplDef& impl) { conf->setTraitImpl(impl); },
        [&](GenTraitImpl& impl) { conf->setTraitImpl(impl); },
        [&](Symbol const&) { PRISM_UNREACHABLE(); }
    }); // clang-format on
}

void ConfAnaContext::analyze(TraitImplDef& impl) {
    doAnalyze(impl.interface());
}

void ConfAnaContext::analyze(GenTraitImplInst& impl) {
    copyInstantiate(*impl.genTemplate(), impl, impl.genArguments(),
                    impl.genTemplate()->genParams());
}

void ConfAnaContext::analyze(GenTraitImpl& impl) {
    doAnalyze(impl.interface());
}

void ConfAnaContext::analyze(CompositeType& type) {
    analyzeObligations(type, type.associatedScope());
    analyzeConformances(type, type.associatedScope());
    if (!type.isCompleteForTraits())
        diagHandler.push<IncompleteImpl>(sourceContext, type.facet(), &type,
                                         type);
}

void ConfAnaContext::analyze(BaseTrait& base) {
    Symbol* parentSym = base.parentScope()->assocSymbol();
    if (auto* type = dyncast<CompositeType*>(parentSym))
        inheritObligations(*base.trait(), *type);
    else if (auto* trait = dyncast<Trait*>(parentSym))
        inheritObligations(*base.trait(), *trait);
}

void ConfAnaContext::analyze(BaseClass& base) {
    auto* parentSymbol = base.parentScope()->assocSymbol();
    auto& parentInterface = getInterfaceLike(*parentSymbol);
    inheritObligations(*base.type(), parentInterface);
}

void prism::analyzeConformances(MonotonicBufferResource& resource,
                                SemaContext& ctx,
                                DiagnosticHandler& diagHandler, Target&,
                                DependencyGraph const& dependencies) {
    for (auto* sym: dependencies.getTopoOrder() | reverse)
        analyzeConformance(resource, ctx, diagHandler, *sym);
}

void prism::analyzeConformance(MonotonicBufferResource&, SemaContext& ctx,
                               DiagnosticHandler& diagHandler, Symbol& sym) {
    ConfAnaContext confCtx{ ctx, diagHandler, getSourceContext(&sym) };
    visit(sym, FN1(&, confCtx.analyze(_1)));
}

static bool specArgMatch(Symbol const* existingArg, Symbol const* newArg) {
    if (existingArg == newArg) return true;
    if (auto* typeParam = dyncast<GenericTypeParam const*>(existingArg)) {
        auto* typeArg = dyncast<ValueType const*>(newArg);
        if (!typeArg) return false;
        return conformsTo(*typeArg, *typeParam->trait());
    }
    return false;
}

static bool isSpecializationOf(Symbol const* spec, Symbol const* general) {
    if (spec == general) return true;
    // clang-format off
    return visit(*spec, csp::overload{
        [&](GenTraitInst const& spec) {
            auto* genInst = dyncast<GenTraitInst const*>(general);
            if (!genInst) return false;
            if (spec.genTemplate() != genInst->genTemplate())
                return false;
            return ranges::all_of(zip(genInst->genArguments(),
                                      spec.genArguments()),
                                  FN1(specArgMatch(_1.first, _1.second)));
        },
        [](Symbol const&) { return false; },
    }); // clang-format on
}

bool prism::conformsTo(ValueType const& type, Trait const& trait) {
    if (trait.name() == "type") // Ugh, how to we fix this?!
        return true;            // All types conform to the `type` trait
    if (auto* compType = dyncast<CompositeType const*>(&type)) {
        if (ranges::any_of(compType->baseTraits(),
                           FN1(&, conformsTo(*_1->trait(), trait))))
            return true;
        if (ranges::any_of(compType->baseClasses(),
                           FN1(&, conformsTo(*_1->type(), trait))))
            return true;
    }
    if (type.findTraitImpl(&trait) != nullptr) return true;
    if (auto* inst = dyncast<GenTraitInst const*>(&trait)) {
        auto impls = type.findTraitImpls(inst->genTemplate());
        for (auto* impl: impls)
            if (isSpecializationOf(&type, impl->conformingType()) &&
                isSpecializationOf(&trait, impl->trait()))
                return true;
    }
    return false;
}

bool prism::conformsTo(Trait const& derived, Trait const& base) {
    if (base.name() == "type") return true;
    if (&derived == &base) return true;
    return ranges::any_of(derived.baseTraits(),
                          FN1(&, conformsTo(*_1->trait(), base)));
}
