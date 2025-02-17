#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("UndeclaredID", "[sema]") {
    auto c = makeDiagChecker(R"(
fn foo(arg: Bar) {}
)");
    CHECK(c.findDiagOnLine<UndeclaredID>(2));
}

TEST_CASE("Redefinition", "[sema]") {
    auto c = makeDiagChecker(R"(
/* 2: */ struct MyType {}
/* 3: */ struct MyType {}
/* 4: */ trait MyTrait {}
/* 5: */ trait MyTrait {}
/* 6: */ var MyVar: i32;
/* 7: */ var MyVar: i32;
/* 8: */ fn MyFunc() -> i32 {}
/* 9: */ fn MyFunc(n: i32) -> i32 {}
)");
    CHECK(c.noDiagOnLine(2));
    CHECK(c.findDiagOnLine<Redefinition>(3));
    CHECK(c.noDiagOnLine(4));
    CHECK(c.findDiagOnLine<Redefinition>(5));
    CHECK(c.noDiagOnLine(6));
    CHECK(c.findDiagOnLine<Redefinition>(7));
    CHECK(c.noDiagOnLine(8));
    CHECK(c.noDiagOnLine(9));
}

TEST_CASE("TypeDefCycle", "[sema]") {
    auto c = makeDiagChecker(R"(
struct Foo { var bar: Bar; }
struct Bar { var foo: Foo; }
)");
    auto* diag = c.findDiag<TypeDefCycle>();
    REQUIRE(diag);
    CHECK(c.findDiagOnLine<SemaNote>(*diag, 2));
    CHECK(c.findDiagOnLine<SemaNote>(*diag, 3));
}

TEST_CASE("BadSymRef", "[sema]") {
    auto c = makeDiagChecker(R"(
struct S: 0 {}
fn foo() { return i32; }
fn foo() -> Global { return ; }
let Global: i32; 
)");
    CHECK(c.findDiagOnLine<BadSymRef>(2));
    CHECK(c.findDiagOnLine<BadSymRef>(3));
    CHECK(c.findDiagOnLine<BadSymRef>(4));
}

TEST_CASE("ThisParamBadPosition", "[sema]") {
    auto c = makeDiagChecker(R"(
struct S {  fn foo(n: i32, &this); }
trait T {  fn foo(n: i32, &this); }
)");
    CHECK(c.findDiagOnLine<ThisParamBadPosition>(2));
    CHECK(c.findDiagOnLine<ThisParamBadPosition>(3));
}

TEST_CASE("ThisParamFreeFunction", "[sema]") {
    auto c = makeDiagChecker(R"(
fn foo(&this) {}
fn bar(this) {}
)");
    CHECK(c.findDiagOnLine<ThisParamFreeFunction>(2));
    CHECK(c.findDiagOnLine<ThisParamFreeFunction>(3));
}

TEST_CASE("AmbiguousConformance", "[sema]") {
    auto c = makeDiagChecker(R"(
trait T1 { fn foo(&this); }
trait T2 { fn foo(&this); }
trait T: T1, T2 { fn foo(&this) {} }
struct S: T1, T2 { fn foo(&this) {} }
)");
    CHECK(c.findDiagOnLine<AmbiguousConformance>(4));
    CHECK(c.findDiagOnLine<AmbiguousConformance>(5));
}

TEST_CASE("IncompleteImpl", "[sema]") {
    auto c = makeDiagChecker(R"(
trait T { fn foo(&this); }
struct S: T {}
struct U {}
impl T for U {}
)");
    CHECK(c.findDiagOnLine<IncompleteImpl>(3));
    CHECK(c.findDiagOnLine<IncompleteImpl>(5));
}

TEST_CASE("DuplicateTraitImpl", "[sema]") {
    auto c = makeDiagChecker(R"(
trait T {}
struct S: T {}
impl T for S {}
)");
    CHECK(c.findDiagOnLine<DuplicateTraitImpl>(4));
}

TEST_CASE("Bad generic instantiation", "[sema]") {
    auto c = makeDiagChecker(R"(
/* 2: */ trait Int32 {}
/* 3: */ struct [T: Int32] IntWrapper {}
/* 4: */ 
/* 7: */ let baz: IntWrapper(i64, 42);
/* 5: */ let foo: IntWrapper(42);
/* 6: */ let bar: IntWrapper(i64);
)");
    CHECK(c.findDiagOnLine<InvalidNumOfGenArgs>(5));
    CHECK(c.findDiagOnLine<BadSymRef>(6));
    CHECK(c.findDiagOnLine<BadGenTypeArg>(7));
}

TEST_CASE("Members and base classes in trait", "[sema]") {
    auto c = makeDiagChecker(R"(
trait T: S {
    var value: i32;
}
struct S {}
)");
    CHECK(c.findDiagOnLine<BaseClassInTrait>(2));
    CHECK(c.findDiagOnLine<MemVarInTrait>(3));
}
