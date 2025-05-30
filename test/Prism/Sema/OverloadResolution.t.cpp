#include <catch2/catch_test_macros.hpp>

#include "Prism/Sema/Symbol.h"
#include "Prism/TestUtils/TestCompiler.h"

using namespace prism;

TEST_CASE("Overload on specificity", "[sema]") {
    auto t = make_inv_tester(R"(
struct [T: type, N: u64] Array {}

fn [T: type, N: u64, M: u64] foo(arg: Array(Array(T, N), M)) -> i32 { 42 }
fn [T: type, N: u64] foo(arg: Array(T, N)) -> i32 { 42 }
fn [T: type] foo(arg: T) -> i32 { 42 }

fn bar(arg: Array(Array(i32, 42), 7)) -> i32 {
    foo(arg)
}
)",
                             { .expect_no_errors = true });
    auto* bar = t.eval<FunctionInst const>("bar");
    REQUIRE(bar);
    auto* body = bar->definition()->body();
    REQUIRE(body);
    auto* yield_inst = body->get_yield_inst();
    REQUIRE(yield_inst);
    auto* call_inst = dyncast<CallInst const*>(yield_inst->operand());
    REQUIRE(call_inst);
    auto* def = cast<FunctionInst const*>(call_inst->callee())->definition();
    REQUIRE(def);
    CHECK(def->num_generic_params() == 3);
}
