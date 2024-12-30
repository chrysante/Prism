#include "Prism/Sema/ConformanceAnalysis.h"

#include <range/v3/view.hpp>

#include "Prism/Common/IssueHandler.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::reverse;

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

    csp::unique_ptr<Obligation> analyzeObligation(Symbol* sym) {
        if (!sym) return nullptr;
        return visit(*sym, FN1(this, analyzeOblImpl(_1)));
    }

    csp::unique_ptr<Obligation> analyzeOblImpl(Symbol const&) {
        return nullptr;
    }

    csp::unique_ptr<Obligation> analyzeOblImpl(Function& func) {
        if (func.params().empty() || !func.params().front()->isThis())
            return nullptr;
        return csp::make_unique<FuncObligation>(&func, func.parentScope()
                                                           ->assocSymbol());
    }

    void analyzeObligations(InterfaceLike& interface, Scope* scope) {
        for (auto* sym: scope->symbols())
            if (auto obl = analyzeObligation(sym))
                interface.addObligation(std::move(obl));
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
            matches.front()->addConformance(&func);
            return;
        }
        // TODO: Handle ambiguities
        PRISM_UNIMPLEMENTED();
    }

    void analyzeConformances(InterfaceLike& interface, Scope* scope) {
        for (auto* sym: scope->symbols())
            analyzeConformance(sym, interface);
    }

    void inherit(InterfaceLike const& base, InterfaceLike& derived) {
        for (auto& [key, list]: base.obligations())
            for (auto* obl: list)
                derived.addObligation(clone(*obl));
    }

    void analyze(Trait& trait) {
        analyzeObligations(trait, trait.associatedScope());
        analyzeConformances(trait, trait.associatedScope());
    }

    void analyze(TraitImpl& impl) {
        inherit(*impl.trait(), impl);
        analyzeConformances(impl, impl.associatedScope());
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
