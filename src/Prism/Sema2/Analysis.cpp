#include "Prism/Sema2/Analysis.h"

#include "Prism/Sema2/FunctionAnalysis.h"

using namespace prism;

Module* prism::analyze_module(SemaContext& ctx, DiagnosticEmitter& DE,
                              std::span<SourceFilePair const> input) {
    auto* mod = construct_sema_ir(ctx, DE, input);
    analyze_functions(ctx, DE, *mod);
    return mod;
}
