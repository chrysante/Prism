#include "Prism/Sema/ConformanceAnalysis.h"

#include <range/v3/view.hpp>

#include "Prism/Common/IssueHandler.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/DependencyGraph.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::reverse;

static csp::unique_ptr<Specification> clone(Specification const* spec) {
    if (!spec) return nullptr;
    return visit<csp::unique_ptr<Specification>>(*spec,
                                                 []<typename T>(T const& spec) {
        return csp::make_unique<T>(spec);
    });
}

template <std::derived_from<Specification> S>
static csp::unique_ptr<S> clone(S const* spec) {
    auto c = clone(static_cast<Specification const*>(spec));
    return csp::unique_ptr<S>(cast<S*>(c.release()));
}

namespace {

struct ConformanceAnalysisContext: AnalysisBase {
    void analyze(Symbol const&) {}

    csp::unique_ptr<Obligation> analyzeObligation(Symbol* sym,
                                                  TraitLike& trait) {
        if (!sym) return nullptr;
        return visit(*sym,
                     [&](auto& sym) { return analyzeOblImpl(sym, trait); });
    }

    csp::unique_ptr<Obligation> analyzeOblImpl(Symbol const&,
                                               TraitLike const&) {
        return nullptr;
    }

    csp::unique_ptr<Obligation> analyzeOblImpl(Function& func,
                                               TraitLike& /*trait*/) {
        return csp::make_unique<FunctionObligation>(&func);
    }

    void analyzeObligations(TraitLike& trait, Scope* scope) {
        for (auto* sym: scope->symbols())
            if (auto obl = analyzeObligation(sym, trait))
                trait.addObligation(std::move(obl));
    }

    void analyzeConformances(ImplLike&, Scope*) {}

    template <typename From, typename To>
    void copy(From& from, To& to) {
        for (auto* obl: from.obligations())
            to.addObligation(clone(obl));
        for (auto* conf: from.conformances())
            to.addConformance(clone(conf));
    }

    void analyze(Trait& trait) {
        analyzeObligations(trait, trait.associatedScope());
        analyzeConformances(trait, trait.associatedScope());
    }

    void analyze(TraitImpl& impl) {
        analyzeConformances(impl, impl.associatedScope());
    }

    void analyze(CompositeType& type) {
        analyzeObligations(type, type.associatedScope());
    }

    void analyze(BaseTrait& base) {
        Symbol& parentSym = *base.parentScope()->assocSymbol();
        if (auto* type = dyncast<CompositeType*>(&parentSym)) {
            copy(*base.trait(), *type);
        }
        else if (auto* trait = dyncast<Trait*>(&parentSym)) {
            copy(*base.trait(), *trait);
        }
    }

    void analyze(BaseClass& base) {
        copy(*base.type(),
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
