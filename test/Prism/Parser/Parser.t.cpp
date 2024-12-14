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
    CHECK(*parseFile("fn test() -> T { T{}; T{} }") == AstSourceFile >> Tree{
        AstFuncDecl >> Tree{
            AstUnqualName,
            AstParamList,
            Identifier,
            AstCompoundExpr >> Tree{
                AstExprStmt >> Tree{
                    CallFacet >> Tree{
                        Identifier, OpenBrace, ExprListFacet, CloseBrace
                    }
                },
                AstYieldStmt >> Tree{
                    CallFacet >> Tree{
                        Identifier, OpenBrace, ExprListFacet, CloseBrace
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
        ExprListFacet >> Tree{
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

// clang-format on
