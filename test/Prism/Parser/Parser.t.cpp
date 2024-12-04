#include <span>
#include <variant>

#include <catch2/catch_test_macros.hpp>

#include "Prism/Parser/ParserTestUtils.h"

using namespace prism;

// clang-format off

TEST_CASE("Parser", "[parser]") {
    using enum AstNodeType;
    using enum FacetType;
    using enum TokenKind;
    CHECK(*parseFile("fn test() -> T { T{}; }") == AstSourceFile >> Tree{
        AstFuncDecl >> Tree{
            AstUnqualName,
            AstParamList,
            Identifier,
            AstCompoundStmt >> Tree{
                AstExprStmt >> Tree{
                    CallFacet >> Tree{
                        Identifier, OpenBrace, ListFacet, CloseBrace
                    }
                }
            },
        }
    });
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

// clang-format on
