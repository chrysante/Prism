#ifndef PRISM_SEMA_ANALYSIS_H
#define PRISM_SEMA_ANALYSIS_H

#include <span>

#include <Prism/Sema/Construction.h>
#include <Prism/Sema/SemaFwd.h>

namespace prism {

class DiagnosticEmitter;
class SemaContext;

/// Top-level sema analysis function
Module* analyze_module(SemaContext& ctx, DiagnosticEmitter& DE,
                       std::span<SourceFilePair const> input);

} // namespace prism

#endif // PRISM_SEMA_ANALYSIS_H
