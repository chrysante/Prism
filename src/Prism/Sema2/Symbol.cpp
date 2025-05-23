#include "Prism/Sema2/Symbol.h"

#include <sstream>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Source/SourceContext.h"

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
    bool first = true;
    for (auto& elem: rng) {
        if (!first) str << separator;
        first = false;
        str << ranges::invoke(proj, elem);
    }
}

Symbol::Symbol(SymbolType sym_type, Facet const* facet, Scope* parent_scope,
               std::string name, ScopeArg scope_arg):
    _sym_type(sym_type),
    _name(std::move(name)),
    _facet(facet),
    _parent(parent_scope),
    _assoc_scope(scope_arg.eval(this)) {}

SourceFile::SourceFile(Facet const* facet, Scope* parent_scope,
                       ScopeArg scope_arg, SourceContext const& source_context):
    Symbol(SymbolType::SourceFile, facet, parent_scope,
           source_context.filepath().string(), scope_arg),
    _source_context(source_context) {}

std::unique_ptr<StructInst> StructDef::make_canonical_type() {
    return std::make_unique<StructInst>(facet()->name(), this);
}

std::unique_ptr<TraitInst> TraitDef::make_canonical_trait() {
    return std::make_unique<TraitInst>(facet()->name(), this);
}

static std::string_view name_proj(Symbol const* symbol) {
    using namespace std::string_view_literals;
    return symbol ? symbol->name() : "NULL"sv;
}

std::string StructInst::make_name() const {
    if (generic_args().empty()) return definition()->name();
    std::stringstream sstr;
    sstr << definition()->name() << "(";
    print_separated(sstr, ", ", generic_args(), name_proj);
    sstr << ")";
    return std::move(sstr).str();
}

void StructInst::verify() const {
    PRISM_ASSERT(definition());
    PRISM_ASSERT(generic_args().size() ==
                 definition()->generic_params().size());
}

std::string TraitInst::make_name() const {
    if (generic_args().empty()) return definition()->name();
    std::stringstream sstr;
    sstr << definition()->name() << "(";
    print_separated(sstr, ", ", generic_args(), name_proj);
    sstr << ")";
    return std::move(sstr).str();
}

void TraitInst::verify() const {
    PRISM_ASSERT(definition());
    PRISM_ASSERT(generic_args().size() ==
                 definition()->generic_params().size());
}
