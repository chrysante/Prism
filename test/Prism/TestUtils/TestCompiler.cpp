#include "Prism/TestUtils/TestCompiler.h"

#include <sstream>
#include <stdexcept>

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
    if (options.expectNoErrors &&
        t.invocation().getDiagnosticHandler().hasErrors())
        throw std::runtime_error("Failed to compile");
    return t;
}

static MonotonicBufferResource gAlloc;

[[noreturn]]
static void throwJitError(std::string_view exprSource,
                          DiagnosticHandler const& diagHandler) {
    std::stringstream sstr;
    sstr << "Failed to jit source fragment: " << exprSource << "\n";
    SourceContext
        ctx; // FIXME: Since all diagnostics have the own source context
             // pointer, we only need this here to satisfy the API
    diagHandler.format(sstr, ctx);
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
    DiagnosticHandler diagHandler;
    auto* facet = parseExpr(gAlloc, *ctx, diagHandler);
    if (diagHandler.hasErrors()) throwJitError(exprSource, diagHandler);
    if (!facet) throw std::runtime_error("No facet");
    auto* symbol =
        analyzeFacet({ invocation().getSemaContext(), diagHandler, ctx }, scope,
                     facet);
    if (!symbol || diagHandler.hasErrors())
        throwJitError(exprSource, diagHandler);
    return symbol;
}
