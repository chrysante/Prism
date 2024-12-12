#define PRISM_IMPL

#include "Prism/Parser/SyntaxIssue.h"

#include <ostream>

#include <utl/streammanip.hpp>

using namespace prism;

static constexpr utl::streammanip lowercase =
    [](std::ostream& str, auto const&... args) { ((str << args), ...); };

#define SYNTAX_ISSUE_DEF(Name, Base, CtorArgs, CtorImpl)                       \
    Name::Name(                                                                \
        PRISM_FOR_EACH(PRISM_PARAM_DECLARE, PRISM_REMOVE_PARENS CtorArgs)):    \
        Base(PRISM_FOR_EACH(PRISM_PARAM_ID, PRISM_REMOVE_PARENS CtorArgs)) {   \
        using enum MessageKind;                                                \
        PRISM_REMOVE_PARENS CtorImpl                                           \
    }
#include <Prism/Parser/SyntaxIssue.def>
