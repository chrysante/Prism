#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema2/SemaDiagnostic.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("UndeclaredID", "[sema]") {
    auto c = make_diag_checker(R"(
fn foo(arg: Bar) {}
)");
    CHECK(c.find_diag_on_line<UndeclaredID>(2));
}

TEST_CASE("Redefinition", "[sema]") {
    auto c = make_diag_checker(R"(
/*  2: */ struct MyType {}
/*  3: */ struct MyType {}
/*  4: */ trait MyTrait {}
/*  5: */ trait MyTrait {}
/*  6: */ var MyVar: i32 = 0;
/*  7: */ var MyVar: i32 = 0;
/*  8: */ fn MyFunc() -> i32 {}
/*  9: */ fn MyFunc(n: i32) -> i32 {}
/* 10: */ fn MyFunc(n: i32) -> i32 {}
)");
    CHECK(c.no_diag_on_line(2));
    CHECK(c.find_diag_on_line<Redefinition>(3));
    CHECK(c.no_diag_on_line(4));
    CHECK(c.find_diag_on_line<Redefinition>(5));
    CHECK(c.no_diag_on_line(6));
    CHECK(c.find_diag_on_line<Redefinition>(7));
    CHECK(c.no_diag_on_line(8));
    CHECK(c.no_diag_on_line(9));
    CHECK(c.find_diag_on_line<FuncRedefinition>(10));
}

TEST_CASE("Function redefinition", "[sema]") {
    auto c = make_diag_checker(R"(
/* 2: */ fn [T: type, U: type] foo(T, U) {}
/* 3: */ fn [A: type, B: type] foo(A, B) {}
/* 4: */ 
/* 5: */ fn [T: type, U: type] bar(T, U) {}
/* 6: */ fn [T: type, U: type] bar(U, T) {}
)");
    CHECK(c.no_diag_on_line(2));
    CHECK(c.find_diag_on_line<FuncRedefinition>(3));
    CHECK(c.no_diag_on_line(5));
    CHECK(c.no_diag_on_line(6));
}

TEST_CASE("Function redefinition 2", "[sema]") {
    auto c = make_diag_checker(R"(
/* 2: */ struct [T: type] S {
/* 3: */     fn foo(T) {}
/* 4: */     fn [T: type] foo(T) {}
/* 5: */     fn [T: type] foo(T) {}
/* 6: */ }
)");
    CHECK(c.no_diag_on_line(3));
    CHECK(c.no_diag_on_line(4));
    CHECK(c.find_diag_on_line<FuncRedefinition>(5));
}

TEST_CASE("BindingMissingTypespec", "[sema]") {
    auto c = make_diag_checker(R"(
let x = 0;
)");
    CHECK(c.find_diag_on_line<BindingMissingTypespec>(2));
}

TEST_CASE("Bad function calls", "[sema]") {
    auto c = make_diag_checker(R"(
fn user(x: f32) {
    foo(i32);    // BadSymRef
    bar(i32);    // BadSymRef
    foo(x);      // BadOperandType
    foo(x, i32); // InvalidNumOfCallArgs
    x();         // SymbolNotCallable
}
fn foo(arg: i32) {}
fn [T: type] bar(arg: T) {}
)");
    CHECK(c.find_diag_on_line<BadSymRef>(3));
    CHECK(c.find_diag_on_line<BadSymRef>(4));
    CHECK(c.find_diag_on_line<BadOperandType>(5));
    CHECK(c.find_diag_on_line<InvalidNumOfCallArgs>(6));
    CHECK(c.find_diag_on_line<SymbolNotCallable>(7));
}

TEST_CASE("Overload resolution errors", "[sema]") {
    auto c = make_diag_checker(R"(
struct [T: type, N: i32] Array {}
fn foo(arg: Array(i32, 7)) {}
fn [T: type] foo(arg: Array(T, 7)) {}
fn user(x: Array(f32, 8)) {
    foo(x);      // NoMatchingFunction
} 
)");
    CHECK(c.find_diag_on_line<NoMatchingFunction>(6));
}

TEST_CASE("InvalidNumOfGenArgs", "[sema]") {
    auto c = make_diag_checker(R"(
struct [T: type, N: i32] Array {}
fn user(arg: Array(i32)) {}
fn user(arg: Array(i32, 7, 7)) {}
)");
    CHECK(c.find_diag_on_line<InvalidNumOfGenArgs>(3));
    CHECK(c.find_diag_on_line<InvalidNumOfGenArgs>(4));
}

TEST_CASE("TypeDefCycle", "[sema]") {
#if 0
    auto c = make_diag_checker(R"(
struct Foo { var bar: Bar; }
struct Bar { var foo: Foo; }
)");
    auto* diag = c.find_diag<TypeDefCycle>();
    REQUIRE(diag);
    CHECK(c.find_diag_on_line<SemaNote>(*diag, 2));
    CHECK(c.find_diag_on_line<SemaNote>(*diag, 3));
#endif
}

TEST_CASE("BadSymRef", "[sema]") {
    auto c = make_diag_checker(R"(
fn [N: i32, M: bar] foo(arg: N) {}
fn bar(arg: i32) -> arg { i32 } 
)");
    CHECK(c.find_diag_on_line<BadSymRef>(2));
    CHECK(c.find_diag_on_line<BadSymRef>(3));
}

TEST_CASE("ThisParamBadPosition", "[sema]") {
#if 0
    auto c = make_diag_checker(R"(
struct S {  fn foo(n: i32, this); }
trait T {  fn foo(n: i32, this); }
)");
    CHECK(c.find_diag_on_line<ThisParamBadPosition>(2));
    CHECK(c.find_diag_on_line<ThisParamBadPosition>(3));
#endif
}

TEST_CASE("ThisParamFreeFunction", "[sema]") {
#if 0
    auto c = make_diag_checker(R"(
fn foo(this) {}
fn bar(this) {}
)");
    CHECK(c.find_diag_on_line<ThisParamFreeFunction>(2));
    CHECK(c.find_diag_on_line<ThisParamFreeFunction>(3));
#endif
}

TEST_CASE("Bad trait functions", "[sema]") {
    auto c = make_diag_checker(R"(
trait [T: type] MyTrait {
    fn [U: type] foo(this, U) -> T; // GenericMemberInTrait
    fn [U: type] foo(U) -> T;       // GenericMemberInTrait + NoThisInTraitFunction 
    fn foo() -> T;                  // NoThisInTraitFunction
    fn foo(this) -> T;              // fine
}
)");
    CHECK(c.find_diag_on_line<GenericMemberInTrait>(3));
    CHECK(c.find_diag_on_line<GenericMemberInTrait>(4));
    CHECK(c.find_diag_on_line<NoThisInTraitFunction>(4));
    CHECK(c.find_diag_on_line<NoThisInTraitFunction>(5));
    CHECK(c.no_diag_on_line(6));
}

TEST_CASE("Bad trait implementation", "[sema]") {
#if 0 // FIXME: this test crashes because analysis order is not topological
    auto c = make_diag_checker(R"(
impl [Size: u64] MyTrait(Array(f64, Size)) for MyType { // IncompleteTraitImpl
    fn foo(this) -> MyTrait(Array(f64, 7)) {}    // UnmatchedTraitImpl (incorrect return type)
    fn bar(this) -> MyTrait(Array(f64, Size)) {} // UnmatchedTraitImpl (incorrect name)
} 
trait [T: type] MyTrait {
    fn foo(this) -> T;
}
struct TyType {}
struct [T: type, Size: u64] Array {} 
)");
    CHECK(c.find_diag_on_line<IncompleteTraitImpl>(2));
    CHECK(c.find_diag_on_line<UnmatchedTraitImpl>(3));
    CHECK(c.find_diag_on_line<UnmatchedTraitImpl>(4));
#endif
}

#if 0

TEST_CASE("IncompleteImpl", "[sema]") {
    auto c = make_diag_checker(R"(
trait T { fn foo(this); }
struct S: T {}
struct U {}
impl T for U {}
)");
    CHECK(c.find_diag_on_line<IncompleteImpl>(3));
    CHECK(c.find_diag_on_line<IncompleteImpl>(5));
}

TEST_CASE("DuplicateTraitImpl", "[sema]") {
    auto c = make_diag_checker(R"(
trait T {}
struct S: T {}
impl T for S {}
)");
    CHECK(c.find_diag_on_line<DuplicateTraitImpl>(4));
}

TEST_CASE("Bad generic instantiation", "[sema]") {
    auto c = make_diag_checker(R"(
/* 2: */ trait Int32 {}
/* 3: */ struct [T: Int32] IntWrapper {}
/* 4: */ 
/* 7: */ let baz: IntWrapper(i64, 42);
/* 5: */ let foo: IntWrapper(42);
/* 6: */ let bar: IntWrapper(i64);
)");
    CHECK(c.find_diag_on_line<InvalidNumOfGenArgs>(5));
    CHECK(c.find_diag_on_line<BadSymRef>(6));
    CHECK(c.find_diag_on_line<BadGenTypeArg>(7));
}

TEST_CASE("Members and base classes in trait", "[sema]") {
    auto c = make_diag_checker(R"(
trait T: S {
    var value: i32;
}
struct S {}
)");
    CHECK(c.find_diag_on_line<BaseClassInTrait>(2));
    CHECK(c.find_diag_on_line<MemVarInTrait>(3));
}

#endif
