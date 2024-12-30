#include "Prism/Sema/ConformanceAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/IssueHandler.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaIssue.h"
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

struct ConformanceAnalysisContext: AnalysisBase {
    void analyze(Symbol const&) {}

    void analyzeObligation(Symbol* sym, InterfaceLike& interface) {
        return visit(*sym, FN1(&, analyzeOblImpl(_1, interface)));
    }

    void analyzeOblImpl(Symbol const&, InterfaceLike&) {}

    void analyzeOblImpl(Function& func, InterfaceLike& interface) {
        if (func.params().empty() || !func.params().front()->isThis()) return;
        auto* owner = func.parentScope()->assocSymbol();
        auto obl = csp::make_unique<FuncObligation>(&func, owner);
        interface.addObligation(std::move(obl), SpecAddMode::Define);
    }

    void analyzeObligations(InterfaceLike& interface, Scope* scope) {
        ranges::for_each(scope->symbols(),
                         FN1(&, analyzeObligation(_1, interface)));
    }

    void analyzeConformance(Symbol* sym, InterfaceLike& interface) {
        if (!sym) return;
        visit(*sym, FN1(&, analyzeConfImpl(_1, interface)));
    }

    void analyzeConfImpl(Symbol const&, InterfaceLike&) {}

    void analyzeConfImpl(FunctionImpl& func, InterfaceLike& interface) {
        if (func.params().empty() || !func.params().front()->isThis()) return;
        auto matches = interface.matchObligation(func.name(), func.signature());
        if (matches.empty()) return;
        if (matches.size() == 1) {
            matches.front()->addConformance(&func, SpecAddMode::Define);
            return;
        }
        auto* issue =
            iss.push<AmbiguousConformance>(sourceContext, func.facet(), &func);
        for (auto* sym: matches | transform(FN1(_1->symbol())))
            issue->addNote(sym->facet(), [=](std::ostream& str) {
                str << "Declaration matches "
                    << formatDecl(sym, { .primaryQualified = true });
            });
    }

    void analyzeConformances(InterfaceLike& interface, Scope* scope) {
        for (auto* sym: scope->symbols())
            analyzeConformance(sym, interface);
    }

    void inherit(InterfaceLike const& base, InterfaceLike& derived) {
        for (auto& [key, list]: base.obligations())
            for (auto* obl: list)
                derived.addObligation(clone(*obl), SpecAddMode::Inherit);
    }

    void analyze(Trait& trait) {
        analyzeObligations(trait, trait.associatedScope());
        analyzeConformances(trait, trait.associatedScope());
    }

    void verifyComplete(InterfaceLike const& interface, Symbol const& symbol) {
        if (interface.isComplete()) return;
        auto* issue =
            iss.push<IncompleteImpl>(sourceContext, symbol.facet(), &symbol);
        for (auto& [key, list]: interface.obligations()) {
            for (auto* obl: list) {
                auto confs = obl->conformances();
                auto* sym = obl->symbol();
                if (confs.empty()) {
                    issue->addNote(sym->facet(), [=](std::ostream& str) {
                        str << "Missing implementation for "
                            << formatDecl(sym, { .primaryQualified = true });
                    });
                    continue;
                }
                if (confs.size() == 1) continue;
                auto* note =
                    issue->addNote(sym->facet(), [=](std::ostream& str) {
                    str << "Multiple implementations for "
                        << formatDecl(sym, { .primaryQualified = true });
                });
                for (auto* conf: confs)
                    note->addNote(conf->facet(), [=](std::ostream& str) {
                        str << "Implemented by "
                            << formatDecl(conf, { .primaryQualified = true });
                    });
            }
        }
    }

    void analyze(TraitImpl& impl) {
        inherit(*impl.trait(), impl);
        analyzeConformances(impl, impl.associatedScope());
        verifyComplete(impl, impl);
    }

    void analyze(CompositeType& type) {
        analyzeObligations(type, type.associatedScope());
    }

    void analyze(BaseTrait& base) {
        Symbol* parentSym = base.parentScope()->assocSymbol();
        if (auto* type = dyncast<CompositeType*>(parentSym))
            inherit(*base.trait(), *type);
        else if (auto* trait = dyncast<Trait*>(parentSym))
            inherit(*base.trait(), *trait);
    }

    void analyze(BaseClass& base) {
        inherit(*base.type(),
                cast<CompositeType&>(*base.parentScope()->assocSymbol()));
    }
};

} // namespace

static SourceContext const* getSourceContext(Symbol const* sym) {
    if (!sym) return nullptr;
    auto* scope = sym->parentScope();
    while (scope) {
        if (auto* sourceFile = dyncast<SourceFile const*>(scope->assocSymbol()))
            return &sourceFile->sourceContext();
        scope = scope->parent();
    }
    return nullptr;
}

static void analyzeConformance(SemaContext& ctx, IssueHandler& iss,
                               Symbol* sym) {
    ConformanceAnalysisContext confCtx{ ctx, iss, getSourceContext(sym) };
    visit(*sym, [&](auto& sym) { confCtx.analyze(sym); });
}

void prism::analyzeConformances(MonotonicBufferResource&, SemaContext& ctx,
                                IssueHandler& iss, Target&,
                                DependencyGraph const& dependencies) {
    for (auto* sym: dependencies.getTopoOrder() | reverse) {
        analyzeConformance(ctx, iss, sym);
    }
}
