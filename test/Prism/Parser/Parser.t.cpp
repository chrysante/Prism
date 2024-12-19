#include <span>
#include <variant>

#include <catch2/catch_test_macros.hpp>

#include "Prism/Parser/ParserTestUtils.h"
#include "Prism/Parser/SyntaxIssue.h"

using enum prism::FacetType;
using enum prism::TokenKind;

using prism::IssueOnLine;
using prism::NullNode;
using prism::parseFacet;
using prism::parseFile;
using prism::Tree;

// clang-format off

TEST_CASE("FuncDecl", "[parser]") {
    CHECK(*parseFile("fn test() -> T { T{}; { T{} } }") == SourceFileFacet >> Tree{
        FuncDeclFacet >> Tree{
            Function,
            Identifier,
            ParamListFacet,
            Arrow,
            Identifier,
            CompoundFacet >> Tree{
                OpenBrace,
                StmtListFacet >> Tree{
                    ExprStmtFacet >> Tree{
                        CallFacet >> Tree{
                            Identifier, OpenBrace, ListFacet, CloseBrace
                        },
                        Semicolon
                    }
                },
                CompoundFacet >> Tree {
                    OpenBrace,
                    StmtListFacet,
                    CallFacet >> Tree{
                        Identifier, OpenBrace, ListFacet, CloseBrace
                    },
                    CloseBrace
                },
                CloseBrace
            },
        }
    });
}

TEST_CASE("Simple expressions", "[parser]") {
    CHECK(*parseFacet("0xff + 42 * ++c") == BinaryFacet >> Tree{
        IntLiteralHex,
        Plus,
        BinaryFacet >> Tree{
            IntLiteralDec,
            Star,
            PrefixFacet >> Tree{ DoublePlus, Identifier },
        }
    });

    CHECK(*parseFacet("f(x, y, z)") == CallFacet >> Tree{
        Identifier,
        OpenParen,
        ListFacet >> Tree{
            Identifier, Identifier, Identifier
        },
        CloseParen
    });

    CHECK(*parseFacet("x < y < z") == BinaryFacet >> Tree{
        BinaryFacet >> Tree{
            Identifier, LeftAngle, Identifier
        },
        LeftAngle,
        Identifier
    });

    CHECK(*parseFacet("x = y = z") == BinaryFacet >> Tree{
        Identifier,
        Equal,
        BinaryFacet >> Tree{
            Identifier, Equal, Identifier
        }
    });
}

TEST_CASE("Conditionals", "[parser]") {
    CHECK(*parseFacet("x ? a + b : y ? c : d") == CondFacet >> Tree{
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
    CHECK(*parseFacet("x ? : b") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        Colon,
        Identifier
    } >> IssueOnLine<prism::ExpectedExpr>(0, 4));

    CHECK(*parseFacet("x ? a b") == CondFacet >> Tree{
        Identifier,
        Question,
        Identifier,
        NullNode,
        Identifier
    } >> IssueOnLine<prism::ExpectedToken>(0, 6));

    CHECK(*parseFacet("x ? a :") == CondFacet >> Tree{
        Identifier,
        Question,
        Identifier,
        Colon,
        NullNode
    } >> IssueOnLine<prism::ExpectedExpr>(0, 7));

    CHECK(*parseFacet("x ? :") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        Colon,
        NullNode
    } >> IssueOnLine<prism::ExpectedExpr>(0, 4)
      >> IssueOnLine<prism::ExpectedExpr>(0, 5));

    CHECK(*parseFacet("x ?") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        NullNode,
        NullNode
    } >> IssueOnLine<prism::ExpectedExpr>(0, 3)
      >> IssueOnLine<prism::ExpectedToken>(0, 3)
      >> IssueOnLine<prism::ExpectedExpr>(0, 3));
}

TEST_CASE("Function types", "[parser]") {
    CHECK(*parseFile("let f: fn (n: int, m: int) -> int = fn (){};") ==
          SourceFileFacet >> Tree{
        VarDeclFacet >> Tree{
            Let,
            Identifier,
            Colon,
            FnTypeFacet >> Tree{
                Function,
                ParamListFacet >> Tree{
                    ParamDeclFacet >> Tree{ Error, Identifier, Colon, Int },
                    ParamDeclFacet >> Tree{ Error, Identifier, Colon, Int }
                },
                Arrow,
                Int
            },
            Equal,
            ClosureFacet >> Tree {
                Function,
                ParamListFacet,
                NullNode,
                NullNode,
                CompoundFacet
            },
            Semicolon
        }
    });

    CHECK(*parseFile("fn foo() -> dyn fn (n: int) -> int { fn $0 }") ==
          SourceFileFacet >> Tree{
        FuncDeclFacet >> Tree{
            Function,
            Identifier,
            ParamListFacet,
            Arrow,
            PrefixFacet >> Tree {
                Dyn,
                FnTypeFacet >> Tree{
                    Function,
                    ParamListFacet >> Tree{
                        ParamDeclFacet >> Tree{ Error, Identifier, Colon, Int },
                    },
                    Arrow,
                    Int
                }
            },
            CompoundFacet >> Tree{
                OpenBrace,
                StmtListFacet,
                ClosureFacet >> Tree{
                    Function,
                    NullNode,
                    NullNode,
                    NullNode,
                    AutoArg
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
                Function,
                ParamListFacet >> Tree{
                    ParamDeclFacet >> Tree{ Error, Identifier, Colon, Int },
                    ParamDeclFacet >> Tree{ Error, Identifier, Colon, Int }
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

    CHECK(*parseFile("let g = fn f(2, $0);") ==
          SourceFileFacet >> Tree{
        VarDeclFacet >> Tree{
            Let,
            Identifier,
            NullNode,
            NullNode,
            Equal,
            ClosureFacet >> Tree{
                Function,
                NullNode,
                NullNode,
                NullNode,
                CallFacet >> Tree{
                    Identifier,
                    OpenParen,
                    ListFacet >> Tree{
                        IntLiteralDec, AutoArg
                    },
                    CloseParen
                }
            },
            Semicolon
        }
    });
}

TEST_CASE("Illformed syntax", "Parser") {
    // We only run this to see if it doesn't crash
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
}

// clang-format on
