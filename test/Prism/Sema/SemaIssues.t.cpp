#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/SemaIssue.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("Undeclared ID", "[sema]") {
    auto c = IssueChecker::Make(R"(
fn foo(arg: Bar) {}
)");
    CHECK(c.findOnLine<UndeclaredID>(2));
}

TEST_CASE("Typedef cycle", "[sema]") {
    auto c = IssueChecker::Make(R"(
struct Foo { var bar: Bar; }
struct Bar { var foo: Foo; }
)");
    auto* diag = c.find<TypeDefCycle>();
    REQUIRE(diag);
    CHECK(c.findOnLine<SemaNote>(*diag, 2));
    CHECK(c.findOnLine<SemaNote>(*diag, 3));
}
