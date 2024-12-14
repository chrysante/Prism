#include <span>
#include <variant>

#include <catch2/catch_test_macros.hpp>

#include "Prism/Parser/ParserTestUtils.h"
#include "Prism/Parser/SyntaxIssue.h"

using enum prism::AstNodeType;
using enum prism::FacetType;
using enum prism::TokenKind;

using prism::IssueOnLine;
using prism::NullNode;
using prism::parseFacet;
using prism::parseFile;
using prism::Tree;

// clang-format off

TEST_CASE("FuncDecl", "[parser]") {
    CHECK(*parseFile("fn test() -> T { T{}; { T{} } }") == AstSourceFile >> Tree{
        AstFuncDecl >> Tree{
            AstUnqualName,
            AstParamList,
            Identifier,
            AstCompoundExpr >> Tree{
                AstExprStmt >> Tree{
                    CallFacet >> Tree{
                        Identifier, OpenBrace, ListFacet, CloseBrace
                    }
                },
                AstYieldStmt >> Tree{
                    CompoundFacet >> Tree {
                        OpenBrace,
                        ListFacet,
                        CallFacet >> Tree{
                            Identifier, OpenBrace, ListFacet, CloseBrace
                        },
                        CloseBrace
                    }
                }
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
    CHECK(*parseFacet("x ? a, b : y ? c : d") == CondFacet >> Tree{
        Identifier,
        Question,
        BinaryFacet >> Tree {
            Identifier, Comma, Identifier
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
    } >> IssueOnLine<prism::ExpectedExpression>(0, 4));

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
    } >> IssueOnLine<prism::ExpectedExpression>(0, 7));

    CHECK(*parseFacet("x ? :") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        Colon,
        NullNode
    } >> IssueOnLine<prism::ExpectedExpression>(0, 4)
      >> IssueOnLine<prism::ExpectedExpression>(0, 5));

    CHECK(*parseFacet("x ?") == CondFacet >> Tree{
        Identifier,
        Question,
        NullNode,
        Error,
        NullNode
    } >> IssueOnLine<prism::ExpectedExpression>(0, 3)
      >> IssueOnLine<prism::ExpectedToken>(0, 3)
      >> IssueOnLine<prism::ExpectedExpression>(0, 3));
}

TEST_CASE("Function types", "[parser]") {
    CHECK(*parseFile("let f: fn (n: int, m: int) -> int;") ==
          AstSourceFile >> Tree{
        AstVarDecl >> Tree{
            AstUnqualName,
            FnTypeFacet >> Tree{
                Function,
                AstParamList >> Tree{
                    AstParamDecl >> Tree{ AstUnqualName, Int },
                    AstParamDecl >> Tree{ AstUnqualName, Int }
                },
                Arrow,
                Int
            },
            NullNode
        }
    });

    CHECK(*parseFile("fn foo() -> dyn fn (n: int) -> int { fn $0 }") ==
          AstSourceFile >> Tree{
        AstFuncDecl >> Tree{
            AstUnqualName,
            AstParamList,
            PrefixFacet >> Tree {
                Dyn,
                FnTypeFacet >> Tree{
                    Function,
                    AstParamList >> Tree{
                        AstParamDecl >> Tree{ AstUnqualName, Int },
                    },
                    Arrow,
                    Int
                }
            },
            AstCompoundExpr >> Tree{
                AstYieldStmt >> Tree{
                    AstClosureExpr >> Tree{
                        NullNode,
                        NullNode,
                        AutoArg
                    }
                }
            }
        }
    });
}

TEST_CASE("Currying", "[parser]") {
    CHECK(*parseFile("let f = fn (x: int, y: int) { x * y };") ==
          AstSourceFile >> Tree{
        AstVarDecl >> Tree{
            AstUnqualName,
            NullNode,
            AstClosureExpr >> Tree {
                AstParamList >> Tree{
                    AstParamDecl >> Tree{ AstUnqualName, Int },
                    AstParamDecl >> Tree{ AstUnqualName, Int }
                },
                NullNode,
                CompoundFacet >> Tree {
                    OpenBrace,
                    ListFacet,
                    BinaryFacet >> Tree {
                        Identifier, Star, Identifier
                    },
                    CloseBrace
                }
            }
        }
    });

    CHECK(*parseFile("let g = fn f(2, $0);") ==
          AstSourceFile >> Tree{
        AstVarDecl >> Tree{
            AstUnqualName,
            NullNode,
            AstClosureExpr >> Tree{
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
            }
        }
    });
}

// clang-format on
