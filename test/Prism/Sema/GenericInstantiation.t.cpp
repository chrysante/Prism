#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/Symbol.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("Generic Instantiation", "[sema]") {
    auto tester = makeInvTester(R"(
trait [To: type] ConvertibleTo {
    fn convert(&this) -> To;
}

struct S {}

impl ConvertibleTo(i32) for S {
    fn convert(&this) -> i32 { 42 }
}
)");
    auto* S = tester.eval<StructType>("S");
    auto* impl = S->findTraitImpl(tester.eval<Trait>("ConvertibleTo(i32)"));
    REQUIRE(impl);
    CHECK(impl->isComplete());
}
