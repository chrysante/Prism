#ifndef PRISM_PARSER_SYNTAXISSUE_H
#define PRISM_PARSER_SYNTAXISSUE_H

#include <Prism/Common/Issue.h>
#include <Prism/Source/SourceLocation.h>
#include <Prism/Source/Token.h>

#include <Prism/Common/MacroUtils.h>

namespace prism {

class SyntaxIssue: public Issue {
public:
    Token token() const { return tok; }

protected:
    explicit SyntaxIssue(Token tok): Issue(tok.index), tok(tok) {}

private:
    Token tok;
};

#define PRISM_PARAM_DECLARE(...)             PRISM_PARAM_DECLARE_IMPL __VA_ARGS__
#define PRISM_PARAM_DECLARE_IMPL(type, name) type name

#define PRISM_PARAM_ID(...)             PRISM_PARAM_ID_IMPL __VA_ARGS__
#define PRISM_PARAM_ID_IMPL(type, name) name

#define SYNTAX_ISSUE_DEF(Name, Base, CtorArgs, CtorImpl)                       \
    class Name: public Base {                                                  \
    public:                                                                    \
        explicit Name(PRISM_FOR_EACH(PRISM_PARAM_DECLARE,                      \
                                     PRISM_REMOVE_PARENS CtorArgs));           \
    };
#include <Prism/Parser/SyntaxIssue.def>

} // namespace prism

#ifndef PRISM_IMPL

#undef PRISM_PARAM_DECLARE
#undef PRISM_PARAM_DECLARE_IMPL
#undef PRISM_PARAM_ID
#undef PRISM_PARAM_ID_IMPL
#include <Prism/Common/MacroUtilsUndef.h>

#endif // PRISM_IMPL

#endif // PRISM_PARSER_SYNTAXISSUE_H
