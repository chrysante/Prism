#include "Prism/TestUtils/TestCompiler.h"

#include <sstream>
#include <stdexcept>

#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Diagnostic/DiagnosticFormat.h"
#include "Prism/Parser/Parser.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/ExprAnalysis.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/Symbol.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

static void doInvoke(Invocation& inv, std::string source,
                     InvocationStage stage) {
    inv.add_source_file("test/file.prism", std::move(source));
    inv.run_until(stage);
}

DiagnosticChecker<Invocation> prism::make_diag_checker(std::string source,
                                                       InvocationStage stage) {
    DiagnosticChecker<Invocation> c;
    doInvoke(c.invocation(), std::move(source), stage);
    return c;
}

InvocationTester prism::make_inv_tester(std::string source,
                                        InvTesterOptions options,
                                        InvocationStage stage) {
    InvocationTester t;
    doInvoke(t.invocation(), std::move(source), stage);
    auto& DE = t.invocation().get_diagnostic_emitter();
    if (options.expect_no_errors && DE.hasErrors()) {
        std::stringstream sstr;
        sstr << "Failed to compile: ";
        print(DE, sstr);
        throw std::runtime_error(std::move(sstr).str());
    }
    return t;
}

static MonotonicBufferResource g_alloc;

[[noreturn]]
static void throw_jit_error(std::string_view expr_source,
                            DiagnosticEmitter const& DE) {
    std::stringstream sstr;
    sstr << "Failed to jit source fragment: " << expr_source << "\n";
    print(DE, sstr);
    throw std::runtime_error(std::move(sstr).str());
}

Symbol* InvocationTester::eval(std::string_view expr_source) {
    auto* global_scope = invocation().get_module()->scope();
    auto* file_scope = [&] {
        auto itr = ranges::find_if(global_scope->symbols(), isa<SourceFile>);
        if (itr == global_scope->symbols().end())
            throw std::runtime_error("Cannot find source file");
        return (*itr)->scope();
    }();
    return eval(file_scope, expr_source);
}

namespace {

struct TrappingDelegate final: FacetAnalysisDelegate {
    void emit_instruction(Instruction&) override { PRISM_UNREACHABLE(); }
};

} // namespace

Symbol* InvocationTester::eval(Scope* scope, std::string_view expr_source) {
    // We just leak this here...
    auto* ctx = allocate<SourceContext>(g_alloc, "test/expr-fragment.prism",
                                        expr_source);
    auto DE = makeDefaultDiagnosticEmitter();
    auto* facet = parseExpr(g_alloc, *ctx, *DE);
    if (DE->hasErrors()) throw_jit_error(expr_source, *DE);
    if (!facet) throw std::runtime_error("No facet");
    TrappingDelegate delegate;
    AnalysisContext ana_context{ invocation().get_sema_context(), *DE, ctx };
    SubContext sub_context;
    auto* symbol =
        analyze_facet(ana_context, delegate, sub_context, scope, facet);
    if (!symbol || DE->hasErrors()) throw_jit_error(expr_source, *DE);
    return symbol;
}
