#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

DiagnosticChecker DiagnosticChecker::Make(std::string source,
                                          InvocationStage stage) {
    DiagnosticChecker c;
    auto& inv = c.invocation;
    inv.addSourceFile("test/file.prism", std::move(source));
    inv.runUntil(stage);
    return c;
}
