#include "Prism/Sema2/Symbol.h"

#include <sstream>

#include <utl/streammanip.hpp>

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

StructDef::StructDef(SemaContext& ctx, Facet const* facet, Scope* parent_scope,
                     std::string name, ScopeArg scope_arg,
                     size_t num_generic_params):
    DeclSymbol(SymbolType::StructDef, facet, parent_scope, std::move(name),
               scope_arg, num_generic_params) {
    if (num_generic_params == 0)
        set_canonical(ctx.get_struct_instantiation(this, {}));
}

TraitDef::TraitDef(SemaContext& ctx, Facet const* facet, Scope* parent_scope,
                   std::string name, ScopeArg scope_arg,
                   size_t num_generic_params):
    DeclSymbol(SymbolType::TraitDef, facet, parent_scope, std::move(name),
               scope_arg, num_generic_params) {
    if (num_generic_params == 0)
        set_canonical(ctx.get_trait_instantiation(this, {}));
}

FunctionDef::FunctionDef(SemaContext& ctx, Facet const* facet,
                         Scope* parent_scope, std::string name,
                         ScopeArg scope_arg, size_t num_generic_params,
                         size_t num_arguments):
    DeclSymbol(SymbolType::FunctionDef, facet, parent_scope, std::move(name),
               scope_arg, num_generic_params),
    _args(num_arguments) {
#if 0
    if (num_generic_params==0) set_canonical(ctx.make<FunctionInst>( !!! ));
#endif
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

static constexpr utl::streammanip Typename = [](std::ostream& str,
                                                Type const* type) {
    if (type)
        str << type->name();
    else
        str << "NULL";
};

std::string FunctionType::make_name(FuncSig const& sig) {
    std::stringstream sstr;
    sstr << "fn (";
    print_separated(sstr, ", ", sig.arguments(), [](FuncArgSpec arg) {
        return utl::streammanip([=](std::ostream& str) {
            str << arg.passing_convention() << " " << Typename(arg.type());
        });
    });
    sstr << ") -> " << Typename(sig.return_type());
    return std::move(sstr).str();
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

Value::~Value() {
    for (auto [user, count]: _users)
        user->on_value_destruction(this);
}

void Value::register_user(User* user) { ++_users[user]; }

void Value::unregister_user(User* user) {
    auto itr = _users.find(user);
    PRISM_ASSERT(itr != _users.end(), "'user' was not a user of this value");
    if (--itr->second == 0) _users.erase(itr);
}

IntLiteral::IntLiteral(Facet const* facet, APInt value, Type const* type):
    Constant(SymbolType::IntLiteral, facet, /* parent_scope: */ nullptr,
             /* name: */ {}, ScopeArg::None, type, Mutability::Const,
             ValueCat::RValue),
    _value(std::move(value)) {}

User::~User() {
    for (auto* op: _operands)
        if (op) op->unregister_user(this);
}

void User::set_operand(size_t index, Value* operand) {
    auto& slot = _operands[index];
    if (slot == operand) return;
    if (slot) slot->unregister_user(this);
    operand->register_user(this);
    slot = operand;
}

void User::register_operands() {
    for (auto* op: _operands)
        if (op) op->register_user(this);
}

void User::on_value_destruction(Value* operand) {
    for (auto& op: _operands)
        if (op == operand) op = nullptr;
}

std::string FunctionInst::make_name() const {
    if (generic_args().empty()) return definition()->name();
    std::stringstream sstr;
    sstr << definition()->name() << "[";
    print_separated(sstr, ", ", generic_args(), name_proj);
    sstr << "](...)";
    return std::move(sstr).str();
}

void FunctionInst::verify() const {
    PRISM_ASSERT(definition());
    PRISM_ASSERT(generic_args().size() ==
                 definition()->generic_params().size());
}
