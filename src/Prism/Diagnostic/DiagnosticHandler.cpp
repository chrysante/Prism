#include "Prism/Diagnostic/DiagnosticHandler.h"

#include <iostream>

#include <range/v3/algorithm.hpp>

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

Diagnostic* DiagnosticHandler::push(std::unique_ptr<Diagnostic> issue) {
    list.push_back(std::move(issue));
    return list.back().get();
}

bool DiagnosticHandler::hasErrors() const {
    return ranges::any_of(*this, FN1(_1.kind() == Diagnostic::Error));
}

void DiagnosticHandler::print(SourceContext const& ctx) {
    format(std::cerr, ctx);
}

void DiagnosticHandler::format(std::ostream& str, SourceContext const&) const {
    for (auto& issue: *this) {
        issue.format(str, nullptr);
    }
}
