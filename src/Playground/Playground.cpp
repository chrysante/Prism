#include <iostream>

#include <Prism/Ast/Ast.h>
#include <Prism/Ast/AstDump.h>
#include <Prism/Common/IssueHandler.h>
#include <Prism/Parser/Parser.h>
#include <Prism/Source/SourceContext.h>

int main() {
    auto source = R"(

fn main(x: TypeA, y: TypeB) {}

)";

    prism::IssueHandler H;
    prism::SourceContext ctx({}, source);
    auto ast = prism::parseSourceFile(ctx, H);
    prism::dumpAst(ast.get(), std::cout);
}
