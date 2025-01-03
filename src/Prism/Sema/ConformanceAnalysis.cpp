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
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Sema/SemaPrint.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::reverse;
using ranges::views::transform;

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

namespace {

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
    void analyze(Symbol const&) {}
    void doAnalyze(TraitImplInterface& interface);
    void analyze(TraitDef& trait);
    void analyze(GenTrait& trait);
    void analyze(TraitImplDef& impl);
    void analyze(GenTraitImpl& impl);
    void analyze(CompositeType& type);
    void analyze(BaseTrait& base);
    void analyze(BaseClass& base);
};

} // namespace

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

void ConfAnaContext::analyze(TraitDef& trait) {
    analyzeObligations(trait, trait.associatedScope());
    analyzeConformances(trait, trait.associatedScope());
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
        if (auto* ex = conf->findTraitImpl(trait)) return ex;
        auto itr = ranges::find(conf->baseTraits(), trait, FN1(_1->trait()));
        return itr != conf->baseTraits().end() ? *itr : nullptr;
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
    visit(impl, [&]<typename T>(T& impl) {
        if constexpr (std::same_as<T, TraitImplDef>)
            conf->setTraitImpl(impl);
        else if constexpr (std::same_as<T, GenTraitImpl>)
            conf->setTraitImpl(impl);
        else
            PRISM_UNREACHABLE();
    });
}

void ConfAnaContext::analyze(TraitImplDef& impl) {
    doAnalyze(impl.interface());
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

static void analyzeConformance(SemaContext& ctx, DiagnosticHandler& diagHandler,
                               Symbol* sym) {
    ConfAnaContext confCtx{ ctx, diagHandler, getSourceContext(sym) };
    visit(*sym, [&](auto& sym) { confCtx.analyze(sym); });
}

void prism::analyzeConformances(MonotonicBufferResource&, SemaContext& ctx,
                                DiagnosticHandler& diagHandler, Target&,
                                DependencyGraph const& dependencies) {
    for (auto* sym: dependencies.getTopoOrder() | reverse)
        analyzeConformance(ctx, diagHandler, sym);
}
