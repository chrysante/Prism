#ifndef PRISM_SEMA2_ANALYSIS_H
#define PRISM_SEMA2_ANALYSIS_H

#include <span>

#include <Prism/Sema2/Construction.h>
#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class DiagnosticEmitter;
class SemaContext;

/// Top-level sema analysis function
Module* analyze_module(SemaContext& ctx, DiagnosticEmitter& DE,
                       std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA2_ANALYSIS_H
