#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/Symbol.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("Simple trait impl", "[sema]") {
    auto tester = makeInvTester(R"(
trait MyTrait {
    fn foo(&this) -> i32;
}

trait MyOtherTrait {
    fn bar(&this) -> i32;
}

struct S: MyOtherTrait {
    fn bar(&this) -> i32 { 7 }
}

impl MyTrait for S {
    fn foo(&this) -> i32 { 42 }
}
)",
                                { .expectNoErrors = true });
    auto* S = tester.eval<StructType>("S");
    CHECK(S->isComplete());
    auto* impl = S->findTraitImpl(tester.eval<Trait>("MyTrait"));
    REQUIRE(impl);
    CHECK(impl->isComplete());
}

TEST_CASE("Impl for generic trait", "[sema]") {
    auto tester = makeInvTester(R"(
trait [To: type] ConvertibleTo {
    fn convert(&this) -> To;
}

struct S {}

impl ConvertibleTo(i32) for S {
    fn convert(&this) -> i32 { 42 }
}
)",
                                { .expectNoErrors = true });
    auto* S = tester.eval<StructType>("S");
    auto* impl = S->findTraitImpl(tester.eval<Trait>("ConvertibleTo(i32)"));
    REQUIRE(impl);
    CHECK(impl->isComplete());
    auto* implScope = impl->associatedScope();
    auto* convert = tester.eval<Function>(implScope, "convert");
    CHECK(convert->parentScope() == implScope);
    auto* thisType = dyncast<ReferenceType const*>(convert->paramAt(0)->type());
    REQUIRE(thisType);
    CHECK(thisType->referred().get() == S);
}
