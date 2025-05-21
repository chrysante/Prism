#include "Prism/Sema2/Symbol.h"

#include <sstream>

#include "Prism/Common/Assert.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"

using namespace prism;

Scope* ScopeArg::eval(Symbol* def_symbol) const {
    // clang-format off
    return value.visit<Scope*>(csp::overload{
        [&](Scope* scope) {
            if (value.index() == 0)
                scope->set_defining_symbol(def_symbol);
            return scope;
        },
        [&](SemaContext* ctx) { return ctx->make_scope(def_symbol); },
        [&](NoneVal const*) { return nullptr; }
    }); // clang-format on
}

template <ranges::range Rng, typename Proj = ranges::identity>
static void print_separated(std::ostream& str, auto const& separator, Rng&& rng,
                            Proj&& proj = {}) {
    bool first = false;
    for (auto& elem: rng) {
        if (!first) str << separator;
        first = false;
        str << ranges::invoke(proj, elem);
    }
}

StructDef::StructDef(Scope* parent_scope, std::string name, ScopeArg scope_arg):
    StructDef(parent_scope, std::move(name), scope_arg,
              std::array<GenericParam, 0>{}) {
    _canonical_type = std::make_unique<StructType>(this);
}

TraitDef::TraitDef(Scope* parent_scope, std::string name, ScopeArg scope_arg):
    TraitDef(parent_scope, std::move(name), scope_arg,
             std::array<GenericParam, 0>{}) {
    _canonical_trait = std::make_unique<TraitInst>(this);
}

std::string StructType::make_name() const {
    if (generic_args().empty()) return definition()->name();
    std::stringstream sstr;
    sstr << definition()->name() << "(";
    print_separated(sstr, ", ", generic_args(), FN1(, "<>"));
    sstr << ")";
    return std::move(sstr).str();
}

void StructType::verify() const {
    PRISM_ASSERT(definition());
    PRISM_ASSERT(generic_args().size() ==
                 definition()->generic_params().size());
}

std::string TraitInst::make_name() const {
    if (generic_args().empty()) return definition()->name();
    std::stringstream sstr;
    sstr << definition()->name() << "(";
    print_separated(sstr, ", ", generic_args(), FN1(, "<>"));
    sstr << ")";
    return std::move(sstr).str();
}

void TraitInst::verify() const {
    PRISM_ASSERT(definition());
    PRISM_ASSERT(generic_args().size() ==
                 definition()->generic_params().size());
}
