#ifndef PRISM_SEMA2_SEMAPRINT_H
#define PRISM_SEMA2_SEMAPRINT_H

#include <iosfwd>

#include <utl/streammanip.hpp>

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

/// Options for declaration formatting
struct FmtDeclOptions {
    bool primary_qualified = true;
    bool secondary_qualified = true;
};

/// \Returns a stream manipulator writing the declaration of \p symbol
utl::vstreammanip<> format_decl(Symbol const& symbol,
                                FmtDeclOptions options = {});

/// \overload for pointers
utl::vstreammanip<> format_decl(Symbol const* symbol,
                                FmtDeclOptions options = {});

/// Options for name formatting
struct FmtNameOptions {
    bool qualified = false;
};

/// \Returns a stream manipulator writing the name of \p symbol
utl::vstreammanip<> format_name(Symbol const& symbol,
                                FmtNameOptions options = {});

/// \overload for pointers
utl::vstreammanip<> format_name(Symbol const* symbol,
                                FmtNameOptions options = {});

} // namespace prism

#endif // PRISM_SEMA2_SEMAPRINT_H
