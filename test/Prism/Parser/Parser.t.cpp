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
#if 0
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
        Error,
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
        Error,
        NullNode
    } >> IssueOnLine<prism::ExpectedExpr>(0, 3)
      >> IssueOnLine<prism::ExpectedToken>(0, 3)
      >> IssueOnLine<prism::ExpectedExpr>(0, 3));
#endif
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

// clang-format on
