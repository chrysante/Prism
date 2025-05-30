#include "Prism/Sema/Analysis.h"

#include "Prism/Sema/ConformanceAnalysis.h"
#include "Prism/Sema/FunctionAnalysis.h"

using namespace prism;

Module* prism::analyze_module(SemaContext& ctx, DiagnosticEmitter& DE,
                              std::span<SourceFilePair const> input) {
    auto* mod = construct_sema_ir(ctx, DE, input);
    analyze_trait_conformances(ctx, DE, *mod);
    analyze_functions(ctx, DE, *mod);
    return mod;
}
