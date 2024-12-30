#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/SemaIssue.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("Undeclared ID", "[sema]") {
    auto c = IssueChecker::Make(R"(
fn foo(arg: Bar) {}
)");
    CHECK(c.find<UndeclaredID>());
}
