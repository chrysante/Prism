#include "Prism/Diagnostic/DiagnosticFormat.h"

#include <iostream>

#include "Prism/Diagnostic/Diagnostic.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"

using namespace prism;

void prism::print(DiagnosticEmitter const& emitter) {
    print(emitter, std::cerr);
}

void prism::print(DiagnosticEmitter const& emitter, std::ostream& str) {
    for (auto* diag: emitter.getAll()) {
        diag->format(str, nullptr);
    }
}
