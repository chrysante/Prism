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
    CHECK(*parseFile("fn test() -> T {}") == AstSourceFile >> Tree{
        AstFuncDecl >> Tree{
            AstUnqualName,
            AstParamList,
            Identifier,
            AstCompoundStmt,
        }
    });
    CHECK(*parseFacet("0xff + 42 * c") == BinaryFacet >> Tree{
        IntLiteralHex,
        BinaryFacet >> Tree{
            IntLiteralDec,
            Identifier,
        }
    });
}

// clang-format on
