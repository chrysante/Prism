#ifndef PRISM_SEMA_SEMAPRINT_H
#define PRISM_SEMA_SEMAPRINT_H

#include <iosfwd>

#include <utl/streammanip.hpp>

#include <Prism/Sema/SymbolFwd.h>

namespace prism {

void print(Symbol const& symbol, std::ostream& ostream);

utl::vstreammanip<> formatDecl(Symbol const& symbol);

} // namespace prism

#endif // PRISM_SEMA_SEMAPRINT_H
