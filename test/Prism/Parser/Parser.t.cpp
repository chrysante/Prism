#include <random>
#include <span>
#include <variant>

#include <catch2/catch_test_macros.hpp>

#include "Prism/Parser/ParserTestUtils.h"
#include "Prism/Parser/SyntaxError.h"

using enum prism::FacetType;
using enum prism::TokenKind;

using prism::DiagnosticOnLine;
using prism::NullNode;
using prism::parseExpr;
using prism::parseFile;
using prism::parseTypeSpec;
using prism::Tree;

// clang-format off

TEST_CASE("FuncDecl", "[parser]") {
    auto* refTree = SourceFileFacet >> Tree{
        FuncDefFacet >> Tree{
            Fn,
            NullNode,
            Identifier,
            ParamListFacet,
            Arrow,
            CallFacet >> Tree{
                Identifier,
                OpenParen,
                ListFacet >> Tree{ CompoundFacet },
                CloseParen
            },
            CompoundFacet >> Tree{
                OpenBrace,
                StmtListFacet >> Tree{
                    ExprStmtFacet >> Tree{
                        AggrConstructFacet >> Tree{
                            Identifier, OpenBrace, ListFacet, CloseBrace
                        },
                        Semicolon
                    }
                },
                CompoundFacet >> Tree{
                    OpenBrace,
                    StmtListFacet,
                    AggrConstructFacet >> Tree{
                        Identifier, OpenBrace, ListFacet, CloseBrace
                    },
                    CloseBrace
                },
                CloseBrace
            },
        }
    };
    CHECK(*parseFile("fn test() -> T({}) { T{}; { T{} } }") == refTree);
}

TEST_CASE("Simple expressions", "[parser]") {
    CHECK(*parseExpr("0xff + 42 * ++c") == BinaryFacet >> Tree{
        IntLiteralHex,
        Plus,
        BinaryFacet >> Tree{
            IntLiteralDec,
            Star,
            PrefixFacet >> Tree{ DoublePlus, Identifier },
        }
    });

    CHECK(*parseExpr("f(x, y, z)") == CallFacet >> Tree{
        Identifier,
        OpenParen,
        ListFacet >> Tree{
            Identifier, Identifier, Identifier
        },
        CloseParen
    });

    CHECK(*parseExpr("x < y < z") == BinaryFacet >> Tree{
        BinaryFacet >> Tree{
            Identifier, LeftAngle, Identifier
        },
        LeftAngle,
        Identifier
    });

    CHECK(*parseExpr("x = y = z") == BinaryFacet >> Tree{
        Identifier,
        Equal,
        BinaryFacet >> Tree{
            Identifier, Equal, Identifier
        }
    });
}

TEST_CASE("Conditionals", "[parser]") {
    CHECK(*parseExpr("x ? a + b : y ? c : d") == CondFacet >> Tree{
        Identifier,
        Question,
        BinaryFacet >> Tree {
            Identifier, Plus, Identifier
        },
        Colon,
        CondFacet >> Tree{
            Identifier,
            Question,
            Identifier,
            Colon,
            Identifier
        }
    });
    CHECK(*parseExpr("x ? : b") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        Colon,
        Identifier
    } >> DiagnosticOnLine<prism::ExpectedExpr>(0, 4));

    CHECK(*parseExpr("x ? a b") == CondFacet >> Tree{
        Identifier,
        Question,
        Identifier,
        NullNode,
        Identifier
    } >> DiagnosticOnLine<prism::ExpectedToken>(0, 6));

    CHECK(*parseExpr("x ? a :") == CondFacet >> Tree{
        Identifier,
        Question,
        Identifier,
        Colon,
        NullNode
    } >> DiagnosticOnLine<prism::ExpectedExpr>(0, 7));

    CHECK(*parseExpr("x ? :") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        Colon,
        NullNode
    } >> DiagnosticOnLine<prism::ExpectedExpr>(0, 4)
      >> DiagnosticOnLine<prism::ExpectedExpr>(0, 5));

    CHECK(*parseExpr("x ?") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        NullNode,
        NullNode
    } >> DiagnosticOnLine<prism::ExpectedExpr>(0, 3)
      >> DiagnosticOnLine<prism::ExpectedToken>(0, 3)
      >> DiagnosticOnLine<prism::ExpectedExpr>(0, 3));
}

TEST_CASE("Binary expressions", "[parser]") {
    CHECK(*parseExpr("x * ") == BinaryFacet >> Tree{
        Identifier,
        Star,
        NullNode,
    } >> DiagnosticOnLine<prism::ExpectedExpr>(0, 4));
    
    CHECK(*parseExpr("x * = x") == BinaryFacet >> Tree{
        Identifier,
        Star,
        Identifier,
    } >> DiagnosticOnLine<prism::UnexpectedToken>(0, 4));
}

TEST_CASE("Function types", "[parser]") {
    CHECK(*parseFile("let f: fn (n: int, m: int) -> int = fn (){};") ==
          SourceFileFacet >> Tree{
        VarDeclFacet >> Tree{
            Let,
            Identifier,
            Colon,
            FnTypeFacet >> Tree{
                Fn,
                ParamListFacet >> Tree{
                    NamedParamDeclFacet >> Tree{ Identifier, Colon, Int },
                    NamedParamDeclFacet >> Tree{ Identifier, Colon, Int }
                },
                Arrow,
                Int
            },
            Equal,
            ClosureFacet >> Tree {
                Fn,
                ParamListFacet,
                NullNode,
                NullNode,
                CompoundFacet
            },
            Semicolon
        }
    });

    CHECK(*parseFile("fn[T: type] foo() -> dyn fn (n: int) -> int { fn @0 }") ==
          SourceFileFacet >> Tree{
        FuncDefFacet >> Tree{
            Fn,
            GenParamListFacet >> Tree{
                GenParamDeclFacet >> Tree{ Identifier, Colon, Type }
            },
            Identifier,
            ParamListFacet,
            Arrow,
            PrefixFacet >> Tree {
                Dyn,
                FnTypeFacet >> Tree{
                    Fn,
                    ParamListFacet >> Tree{
                        NamedParamDeclFacet >> Tree{ Identifier, Colon, Int },
                    },
                    Arrow,
                    Int
                }
            },
            CompoundFacet >> Tree{
                OpenBrace,
                StmtListFacet,
                ClosureFacet >> Tree{
                    Fn,
                    NullNode,
                    NullNode,
                    NullNode,
                    AutoArgFacet
                },
                CloseBrace
            }
        }
    });
}

TEST_CASE("Currying", "[parser]") {
    CHECK(*parseFile("let f = fn (x: int, y: int) { x * y };") ==
          SourceFileFacet >> Tree{
        VarDeclFacet >> Tree{
            Let,
            Identifier,
            NullNode,
            NullNode,
            Equal,
            ClosureFacet >> Tree {
                Fn,
                ParamListFacet >> Tree{
                    NamedParamDeclFacet >> Tree{ Identifier, Colon, Int },
                    NamedParamDeclFacet >> Tree{ Identifier, Colon, Int }
                },
                NullNode,
                NullNode,
                CompoundFacet >> Tree {
                    OpenBrace,
                    StmtListFacet,
                    BinaryFacet >> Tree {
                        Identifier, Star, Identifier
                    },
                    CloseBrace
                }
            },
            Semicolon
        }
    });

    CHECK(*parseFile("let g = fn f(2, @0);") ==
          SourceFileFacet >> Tree{
        VarDeclFacet >> Tree{
            Let,
            Identifier,
            NullNode,
            NullNode,
            Equal,
            ClosureFacet >> Tree{
                Fn,
                NullNode,
                NullNode,
                NullNode,
                CallFacet >> Tree{
                    Identifier,
                    OpenParen,
                    ListFacet >> Tree{
                        IntLiteralDec, AutoArgFacet
                    },
                    CloseParen
                }
            },
            Semicolon
        }
    });
}

TEST_CASE("Expressions nested in type specs", "[parser]") {
    CHECK(*parseExpr("fn -> T(int{}) @0") == ClosureFacet >> Tree{
        Fn,
        NullNode,
        Arrow,
        CallFacet >> Tree{
            Identifier,
            OpenParen,
            ListFacet >> Tree{
                AggrConstructFacet >> Tree {
                    Int,
                    OpenBrace,
                    ListFacet,
                    CloseBrace
                },
            },
            CloseParen
        },
        AutoArgFacet
    });
}

TEST_CASE("Deduction qualifiers", "[parser]") {
    CHECK(*parseTypeSpec("&mut") == PrefixFacet >> Tree{
        Ampersand, Mut
    });
}

TEST_CASE("Auto arguments", "[parser]") {
    CHECK(*parseExpr("@0") == AutoArgFacet >> Tree{
        AutoArgIntro, NullNode, NullNode, NullNode
    });
    CHECK(*parseExpr("@0arg") == AutoArgFacet >> Tree{
        AutoArgIntro, Identifier, NullNode, NullNode
    });
    CHECK(*parseExpr("@0:&") == AutoArgFacet >> Tree{
        AutoArgIntro, NullNode, Colon, Ampersand
    });
    CHECK(*parseExpr("@0:int") == AutoArgFacet >> Tree{
        AutoArgIntro, NullNode, Colon, Int
    });
    CHECK(*parseExpr("@0arg:int") == AutoArgFacet >> Tree{
        AutoArgIntro, Identifier, Colon, Int
    });
    CHECK(*parseExpr("fn @0arg:& (arg)") == ClosureFacet >> Tree{
        Fn,
        NullNode,
        NullNode,
        NullNode,
        CallFacet >> Tree{
            AutoArgFacet >> Tree{
                AutoArgIntro, Identifier, Colon, Ampersand
            },
            OpenParen,
            ListFacet >> Tree{ Identifier },
            CloseParen
        }
    });
}

TEST_CASE("Trait implementation", "[parser]") {
    auto* typeImpl = parseFile(R"(
impl [R: type, M: MyTrait] std.function for MyFunction {}
)");
    CHECK(*typeImpl == SourceFileFacet >> Tree{
        TraitImplFacet >> Tree{
            Impl,
            GenParamListFacet >> Tree{
                GenParamDeclFacet >> Tree{ Identifier, Colon, Type },
                GenParamDeclFacet >> Tree{ Identifier, Colon, Identifier }
            },
            TraitImplTypeFacet >> Tree{
                BinaryFacet >> Tree{ Identifier, Period, Identifier },
                For,
                Identifier,
                OpenBrace,
                MemberListFacet,
                CloseBrace
            }
        }
    });

    auto* funcImpl = parseFile(R"(
impl fn std.function.call(&this, n: int, m: int) -> int for MyFunction {}
)");
    CHECK(*funcImpl == SourceFileFacet >> Tree{
        TraitImplFacet >> Tree{
            Impl,
            NullNode,
            TraitImplFuncFacet >> Tree{
                FuncDeclFacet,
                For,
                Identifier,
                CompoundFacet
            }
        }
    });
}

// clang-format on

static std::vector<char> makeRandomBits(uint64_t seed, size_t count) {
    std::mt19937_64 rng(seed);
    std::uniform_int_distribution<uint32_t> dist(0, 255);
    return ranges::views::generate([&] { return (char)dist(rng); }) |
           ranges::views::take(count) | ranges::to<std::vector>;
}

TEST_CASE("Ill-formed syntax", "Parser") {
    // We only run these to make sure it doesn't crash
    parseFile(R"(
var x = 10 + (20 * 30) / "string_literal" ? true : false && this != null;
fn test() {
    let y = { x << 5; if (y >= 0) return y; else while (x--) x += 1; }
    struct MyStruct {
        trait TraitName {
            fn method(arg: dyn &mut Type) -> int;
        }
    }
}
if (true && false || x > 42) {
    var a = x ? y : z;
    var b = [1, 2, 3];
    let c = "Hello" + "World!";
    fn inlineArrow -> { return new myObject(123); }
}
do { let flag = !false; move flag; } while (count-- > 0);
x += y ? a.b(c, d) : x[0] * x[1];
return x || y && !(z >= (a + b - c * d / e));
)");
    parseFile(R"(
var x = (10 + 20 * { "unterminated_string 
    fn } broken { -> struct ;;; if return 42 } let [
[true false && || || ! != == ??? <:> +-*/ ** ,,, move dyn trait ?:
do { while { fn } x + y ]]]]]]]] "string_literal \ false == struct void int
    x---> new let if else for ,,,,,, %%%%%% :: ( ) } { [ { let var 10e10 fn return!!!
        autoarg ]][intliteraldec]];;; mut dyn !!!!
        return x < <= <<= >= >>> ->=> *== "hello + world
        this += [new:::: struct void if trait true false double fn () } !! ?}
"unterminated again fn fn }}} {;;; ;;; ... continue ! end!!!
)");
    parseFile(R"(
gB6z! $9jK* 1pS@qL^ 2fVx +T_wA& 7Y5Z c%nD4 hJ3eM0 tRb8oNwU 6gQ+ ;Fz#v @H9^ Pj7! B
L0z Xv3QZb F*V4W1r_2G#l8u@ Y5d9s OiJ6k T x p N+Mw C&zA% Ht0Rj7w Kq9b F3z$ L!cJ
PqB4#8 O^u7!9VzH2l* F1yAt5p3o D%_Tn GkWbX 6QNjCw M%Yz0p 1R+5Lz4 tS9fVk
+* KqYdN!7r 3p5Bzx F*W0_9l G4Oa2Tc H8s& JQ%vZb6y M1nRj9P +V7fL 0gTz8u
)");
    auto data = makeRandomBits(42, 1024);
    parseFile({ data.data(), data.size() });
}
