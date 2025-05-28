#include <Prism/Sema2/SemaFwd.h>

namespace prism {

class DiagnosticEmitter;

void analyze_trait_conformances(SemaContext& ctx, DiagnosticEmitter& DE,
                                Module& mod);

} // namespace prism
