#include "Prism/Diagnostic/DiagnosticEmitter.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/Diagnostic.h"

using namespace prism;
using ranges::views::transform;

namespace {

struct DefaultDiagnosticEmitter: DiagnosticEmitter {
    Diagnostic* emit(std::unique_ptr<Diagnostic> diag) override {
        PRISM_EXPECT(diag != nullptr);
        elems.push_back(std::move(diag));
        return elems.back().get();
    }

    bool hasErrors() const override {
        return ranges::any_of(elems, FN1(_1->kind() == Diagnostic::Error));
    }

    bool empty() const override { return elems.empty(); }

    void clear() override { elems.clear(); }

    std::vector<Diagnostic const*> getAll() const override {
        return elems | transform(ToAddress) | ranges::to<std::vector>;
    }

    std::vector<std::unique_ptr<Diagnostic>> elems;
};

struct TrappingDiagnosticEmitter: DiagnosticEmitter {
    Diagnostic* emit(std::unique_ptr<Diagnostic>) override {
        PRISM_UNREACHABLE();
    }

    bool hasErrors() const override { return false; }

    /// \Returns true if no diagnostics have been emitted
    bool empty() const override { return true; }

    void clear() override {}

    std::vector<Diagnostic const*> getAll() const override { return {}; }
};

} // namespace

std::unique_ptr<DiagnosticEmitter> prism::makeDefaultDiagnosticEmitter() {
    return std::make_unique<DefaultDiagnosticEmitter>();
}

std::unique_ptr<DiagnosticEmitter> prism::makeTrappingDiagnosticEmitter() {
    return std::make_unique<TrappingDiagnosticEmitter>();
}
