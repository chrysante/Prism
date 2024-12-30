#ifndef PRISM_SEMA_SEMAPRINT_H
#define PRISM_SEMA_SEMAPRINT_H

#include <iosfwd>

#include <utl/streammanip.hpp>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

struct SemaPrintOptions {
    bool structureMemoryLayout = false;
    bool traitObligations = false;
};

///
void print(Symbol const& symbol, std::ostream& ostream,
           SemaPrintOptions options = {});

/// Prints to `std::cerr`
void print(Symbol const& symbol);

/// Options for declaration formatting
struct FmtDeclOptions {
    bool primaryQualified = false;
    bool secondaryQualified = false;
};

/// \Returns a stream manipulator writing the declaration of \p symbol
utl::vstreammanip<> formatDecl(Symbol const& symbol,
                               FmtDeclOptions options = {});

/// \overload for pointers
utl::vstreammanip<> formatDecl(Symbol const* symbol,
                               FmtDeclOptions options = {});

/// Options for name formatting
struct FmtNameOptions {
    bool qualified = false;
};

/// \Returns a stream manipulator writing the name of \p symbol
utl::vstreammanip<> formatName(Symbol const& symbol,
                               FmtNameOptions options = {});

/// \overload for pointers
utl::vstreammanip<> formatName(Symbol const* symbol,
                               FmtNameOptions options = {});

} // namespace prism

#endif // PRISM_SEMA_SEMAPRINT_H
