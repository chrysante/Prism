#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/ConformanceAnalysis.h"
#include "Prism/Sema/SemaDiagnostic.h"
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
    auto* MyTrait = tester.eval<Trait>("MyTrait");
    auto* impl = S->findTraitImpl(MyTrait);
    REQUIRE(impl);
    CHECK(impl->isComplete());
    auto* MyOtherTrait = tester.eval<Trait>("MyOtherTrait");
    CHECK(conformsTo(*S, *MyTrait));
    CHECK(conformsTo(*S, *MyOtherTrait));
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
    auto* implScope = impl->traitImpl().associatedScope();
    auto* convert = tester.eval<Function>(implScope, "convert");
    CHECK(convert->parentScope() == implScope);
    auto* thisType = dyncast<ReferenceType const*>(convert->paramAt(0)->type());
    REQUIRE(thisType);
    CHECK(thisType->referred().get() == S);
}

TEST_CASE("Generic trait implementation", "[sema]") {
    auto tester = makeInvTester(R"(
trait [To: type] ConvertibleTo {
    fn convert(&this) -> To;
}

struct S {}

trait Int32 {}

impl Int32 for i32 {}

impl [T: Int32] ConvertibleTo(T) for S {
    fn convert(&this) -> T {}
}
)",
                                { .expectNoErrors = true });
    auto* S = tester.eval<StructType>("S");
    auto* convToI32 = tester.eval<Trait>("ConvertibleTo(i32)");
    CHECK(conformsTo(*S, *convToI32));
    auto* convToI64 = tester.eval<Trait>("ConvertibleTo(i64)");
    CHECK(!conformsTo(*S, *convToI64));
}

TEST_CASE("Order independent conformance analysis", "[sema]") {
    auto tester = makeInvTester(R"(
var s: S(i32);
var t: S(i64);
impl Integral for i32 {}
struct [T: Integral] S {}
trait Integral {}
)");
    CHECK(tester.findDiagOnLine<BadGenTypeArg>(3));
    auto* s = tester.eval<Variable>("s");
    CHECK(s->type().get() == tester.eval("S(i32)"));
}
