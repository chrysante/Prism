#ifndef PRISM_SEMA_QUALTYPE_H
#define PRISM_SEMA_QUALTYPE_H

#include <utility>

#include <utl/ipp.hpp>

#include <Prism/Sema/SemaFwd.h>

namespace prism {

/// Const/mutable qualified type
class QualType {
public:
    QualType(): QualType(nullptr, {}) {}

    QualType(Type const* type, Mutability mut): p(type, (unsigned)mut) {}

    /// Static constructor for const qualtypes
    static QualType Const(Type const* type) {
        return QualType(type, Mutability::Const);
    }

    /// Static constructor for mutable qualtypes
    static QualType Mut(Type const* type) {
        return QualType(type, Mutability::Mut);
    }

    /// \Return the type pointer
    Type const* get() const { return p.pointer(); }

    /// \Returns the mutability qualifier
    Mutability mutability() const { return (Mutability)(p.integer()); }

    bool isMut() const { return mutability() == Mutability::Mut; }

    bool isConst() const { return mutability() == Mutability::Const; }

private:
    utl::ipp<Type const*, unsigned, 1> p;
};

} // namespace prism

#endif // PRISM_SEMA_QUALTYPE_H
