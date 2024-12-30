#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("UndeclaredID", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
fn foo(arg: Bar) {}
)");
    CHECK(c.findOnLine<UndeclaredID>(2));
}

TEST_CASE("TypeDefCycle", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
struct Foo { var bar: Bar; }
struct Bar { var foo: Foo; }
)");
    auto* diag = c.find<TypeDefCycle>();
    REQUIRE(diag);
    CHECK(c.findOnLine<SemaNote>(*diag, 2));
    CHECK(c.findOnLine<SemaNote>(*diag, 3));
}

TEST_CASE("BadSymRef", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
struct S: 0 {}
fn foo() { return i32; }
fn foo() -> Global { return ; }
let Global: i32; 
)");
    CHECK(c.findOnLine<BadSymRef>(2));
    CHECK(c.findOnLine<BadSymRef>(3));
    CHECK(c.findOnLine<BadSymRef>(4));
}

TEST_CASE("ThisParamBadPosition", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
struct S {  fn foo(n: i32, &this); }
trait T {  fn foo(n: i32, &this); }
)");
    CHECK(c.findOnLine<ThisParamBadPosition>(2));
    CHECK(c.findOnLine<ThisParamBadPosition>(3));
}

TEST_CASE("AmbiguousConformance", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
trait T1 { fn foo(&this); }
trait T2 { fn foo(&this); }
trait T: T1, T2 { fn foo(&this) {} }
struct S: T1, T2 { fn foo(&this) {} }
)");
    CHECK(c.findOnLine<AmbiguousConformance>(4));
    CHECK(c.findOnLine<AmbiguousConformance>(5));
}

TEST_CASE("IncompleteImpl", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
trait T { fn foo(&this); }
struct S: T {}
struct U {}
impl T for U {}
)");
    CHECK(c.findOnLine<IncompleteImpl>(3));
    CHECK(c.findOnLine<IncompleteImpl>(5));
}

TEST_CASE("DuplicateTraitImpl", "[sema]") {
    auto c = DiagnosticChecker::Make(R"(
trait T {}
struct S: T {}
impl T for S {}
)");
    CHECK(c.findOnLine<DuplicateTraitImpl>(4));
}
