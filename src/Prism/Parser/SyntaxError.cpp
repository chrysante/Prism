#define PRISM_IMPL

#include "Prism/Parser/SyntaxError.h"

#include <ostream>

#include <utl/streammanip.hpp>

using namespace prism;

static constexpr utl::streammanip lowercase =
    [](std::ostream& str, auto const&... args) { ((str << args), ...); };

#define SYNTAX_ISSUE_DEF(Name, Base, CtorArgs, FmtImpl)                        \
    void Name::doFormat(std::ostream& str, SourceContext const& ctx) const {   \
        PRISM_REMOVE_PARENS FmtImpl                                            \
    }
#include <Prism/Parser/SyntaxError.def>
