#ifndef PRISM_SEMA2_SEMAPRINT_H
#define PRISM_SEMA2_SEMAPRINT_H

#include <ostream>

#include <Prism/Sema2/SemaFwd.h>

namespace prism {

struct SemaPrintOptions {
    bool pretty_print_instructions = true;
};

/// Prints \p symbol as a tree to \p ostr
void print(Symbol const& symbol, std::ostream& ostr,
           SemaPrintOptions const& options = {});

/// Prints to `std::cerr`
void print(Symbol const& symbol);

} // namespace prism

#endif // PRISM_SEMA2_SEMAPRINT_H
