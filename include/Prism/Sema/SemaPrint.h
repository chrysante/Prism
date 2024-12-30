#ifndef PRISM_SEMA_SEMAPRINT_H
#define PRISM_SEMA_SEMAPRINT_H

#include <iosfwd>

#include <utl/streammanip.hpp>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

struct SemaPrintOptions {
    bool structureMemoryLayout = false;
    bool traitObligations = false;
    bool traitConformances = false;
};

///
void print(Symbol const& symbol, std::ostream& ostream,
           SemaPrintOptions options = {});

/// Prints to `std::cerr`
void print(Symbol const& symbol);

/// \Returns a stream manipulator writing the declaration of \p symbol
utl::vstreammanip<> formatDecl(Symbol const& symbol);

/// \overload for pointers
utl::vstreammanip<> formatDecl(Symbol const* symbol);

/// \Returns a stream manipulator writing the name of \p symbol
utl::vstreammanip<> formatName(Symbol const& symbol);

/// \overload for pointers
utl::vstreammanip<> formatName(Symbol const* symbol);

} // namespace prism

#endif // PRISM_SEMA_SEMAPRINT_H
