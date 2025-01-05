#include "Prism/TestUtils/TestCompiler.h"

#include <sstream>
#include <stdexcept>

#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Diagnostic/DiagnosticFormat.h"
#include "Prism/Parser/Parser.h"
#include "Prism/Sema/AnalysisBase.h"
#include "Prism/Sema/ExprAnalysis.h"
#include "Prism/Sema/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

static void doInvoke(Invocation& inv, std::string source,
                     InvocationStage stage) {
    inv.addSourceFile("test/file.prism", std::move(source));
    inv.runUntil(stage);
}

DiagnosticChecker<Invocation> prism::makeDiagChecker(std::string source,
                                                     InvocationStage stage) {
    DiagnosticChecker<Invocation> c;
    doInvoke(c.invocation(), std::move(source), stage);
    return c;
}

InvocationTester prism::makeInvTester(std::string source,
                                      InvTesterOptions options,
                                      InvocationStage stage) {
    InvocationTester t;
    doInvoke(t.invocation(), std::move(source), stage);
    auto& DE = t.invocation().getDiagnosticEmitter();
    if (options.expectNoErrors && DE.hasErrors()) {
        std::stringstream sstr;
        sstr << "Failed to compile: ";
        print(DE, sstr);
        throw std::runtime_error(std::move(sstr).str());
    }
    return t;
}

static MonotonicBufferResource gAlloc;

[[noreturn]]
static void throwJitError(std::string_view exprSource,
                          DiagnosticEmitter const& DE) {
    std::stringstream sstr;
    sstr << "Failed to jit source fragment: " << exprSource << "\n";
    print(DE, sstr);
    throw std::runtime_error(std::move(sstr).str());
}

Symbol* InvocationTester::eval(std::string_view exprSource) {
    auto* globalScope = invocation().getTarget()->associatedScope();
    auto* fileScope = [&] {
        auto itr = ranges::find_if(globalScope->symbols(), isa<SourceFile>);
        if (itr == globalScope->symbols().end())
            throw std::runtime_error("Cannot find source file");
        return (*itr)->associatedScope();
    }();
    return eval(fileScope, exprSource);
}

Symbol* InvocationTester::eval(Scope* scope, std::string_view exprSource) {
    // We just leak this here...
    auto* ctx = new SourceContext("test/expr-fragment.prism", exprSource);
    auto DE = makeDefaultDiagnosticEmitter();
    auto* facet = parseExpr(gAlloc, *ctx, *DE);
    if (DE->hasErrors()) throwJitError(exprSource, *DE);
    if (!facet) throw std::runtime_error("No facet");
    auto* symbol =
        analyzeFacet({ invocation().getSemaContext(), *DE, ctx }, scope, facet);
    if (!symbol || DE->hasErrors()) throwJitError(exprSource, *DE);
    return symbol;
}
