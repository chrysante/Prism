#include "Prism/Parser/Parser.h"

#include <cstdint>

#include "Prism/Common/IssueHandler.h"
#include "Prism/Lexer/Lexer.h"

using namespace prism;

using enum TokenKind;

namespace {

struct Parser {
    explicit Parser(std::string_view source, IssueHandler& iss):
        lexer(source, iss), iss(iss) {}

    csp::unique_ptr<AstSourceFile> run();

    csp::unique_ptr<AstDecl> parseDecl();
    csp::unique_ptr<AstFuncDecl> parseFuncDecl(Token declarator);

    std::optional<Token> match(TokenKind tok,
                               std::same_as<TokenKind> auto... rest);
    Token peek();
    Token eat();

    std::optional<Token> peekToken;
    Lexer lexer;
    IssueHandler& iss;
};

} // namespace

csp::unique_ptr<AstSourceFile> prism::parseSourceFile(std::string_view source,
                                                      IssueHandler& iss) {
    return Parser(source, iss).run();
}

csp::unique_ptr<AstSourceFile> Parser::run() {
    utl::small_vector<csp::unique_ptr<AstDecl>> decls;
    while (true) {
        auto decl = parseDecl();
        if (!decl) {
            return csp::make_unique<AstSourceFile>(std::move(decls));
        }
        decls.push_back(std::move(decl));
    }
}

csp::unique_ptr<AstDecl> Parser::parseDecl() {
    if (auto declarator = match(Function)) {
        return parseFuncDecl(*declarator);
    }
    return nullptr;
}

csp::unique_ptr<AstFuncDecl> Parser::parseFuncDecl(Token declarator) {}

std::optional<Token> Parser::match(TokenKind kind,
                                   std::same_as<TokenKind> auto... rest) {
    if (peek().kind == kind || (... || (peek().kind == rest))) {
        return eat();
    }
    return std::nullopt;
}

Token Parser::peek() {
    if (peekToken) {
        return *peekToken;
    }
    auto tok = lexer.next();
    peekToken = tok;
    return tok;
}

Token Parser::eat() {
    if (peekToken) {
        auto tok = *peekToken;
        peekToken.reset();
        return tok;
    }
    return lexer.next();
}
