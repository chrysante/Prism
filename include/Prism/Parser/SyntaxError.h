#ifndef PRISM_PARSER_SYNTAXERROR_H
#define PRISM_PARSER_SYNTAXERROR_H

#include <Prism/Common/Issue.h>
#include <Prism/Source/SourceLocation.h>
#include <Prism/Source/Token.h>

#include <Prism/Common/MacroUtils.h>

namespace prism {

/// Base class of all syntax issues
class SyntaxError: public Issue {
protected:
    explicit SyntaxError(SourceContext const& sourceContext, Token tok):
        Issue(Issue::Error, tok.getSourceRange(), &sourceContext) {}
};

} // namespace prism

// Definition of all derived syntax issue classes. The classes are defined in a
// concise manner in SyntaxError.def

#define PRISM_PARAM_DECLARE(...)             PRISM_PARAM_DECLARE_IMPL __VA_ARGS__
#define PRISM_PARAM_DECLARE_IMPL(type, name) type name

#define PRISM_PARAM_INIT(...)             PRISM_PARAM_INIT_IMPL __VA_ARGS__
#define PRISM_PARAM_INIT_IMPL(type, name) , name(name)

#define PRISM_MEMBER_DECLARE(...)             PRISM_MEMBER_DECLARE_IMPL __VA_ARGS__
#define PRISM_MEMBER_DECLARE_IMPL(type, name) type name;

namespace prism {

#define SYNTAX_ISSUE_DEF(Name, Base, CtorArgs, FmtImpl)                        \
    class Name: public Base {                                                  \
    public:                                                                    \
        explicit Name(SourceContext const& sourceContext,                      \
                      PRISM_FOR_EACH(PRISM_PARAM_DECLARE, PRISM_COMMA,         \
                                     PRISM_REMOVE_PARENS CtorArgs)):           \
            Base(sourceContext, tok)                                           \
                PRISM_FOR_EACH(PRISM_PARAM_INIT, PRISM_NONE,                   \
                               PRISM_REMOVE_PARENS CtorArgs) {}                \
                                                                               \
    private:                                                                   \
        void header(std::ostream& os,                                          \
                    [[maybe_unused]] SourceContext const* ctx) const override; \
                                                                               \
        PRISM_FOR_EACH(PRISM_MEMBER_DECLARE, PRISM_NONE,                       \
                       PRISM_REMOVE_PARENS CtorArgs)                           \
    };
#include <Prism/Parser/SyntaxError.def>

} // namespace prism

#ifndef PRISM_IMPL

#undef PRISM_PARAM_DECLARE
#undef PRISM_PARAM_DECLARE_IMPL
#undef PRISM_PARAM_INIT
#undef PRISM_PARAM_INIT_IMPL
#undef PRISM_MEMBER_DECLARE
#undef PRISM_MEMBER_DECLARE_IMPL
#include <Prism/Common/MacroUtilsUndef.h>

#endif // PRISM_IMPL

#endif // PRISM_PARSER_SYNTAXERROR_H
