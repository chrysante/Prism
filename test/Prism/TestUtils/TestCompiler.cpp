#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

IssueChecker IssueChecker::Make(std::string source, InvocationStage stage) {
    IssueChecker c;
    auto& inv = c.invocation;
    inv.addSourceFile("test/file.prism", std::move(source));
    inv.runUntil(stage);
    return c;
}
