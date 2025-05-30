#include "Prism/Sema2/Symbol.h"

#include <sstream>

#include <range/v3/view.hpp>
#include <utl/strcat.hpp>
#include <utl/streammanip.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;

using ranges::views::transform;

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

DeclSymbol const* TraitImplDef::find_impl_for(
    DeclSymbol const* trait_decl) const {
    PRISM_ASSERT(trait_decl->parent_scope()->defining_symbol() ==
                 cast<TraitInst const*>(trait())->definition());
    auto itr = _conformance_map.find(trait_decl);
    return itr != _conformance_map.end() ? itr->second : nullptr;
}

FuncSig FunctionDef::make_signature() const {
    auto to_spec = [](FunctionArgument const* arg) {
        if (!arg) return FuncArgSpec();
        return arg->make_spec();
    };
    return FuncSig(arguments() | transform(to_spec) | ToSmallVector<>,
                   return_type());
}

bool FunctionDef::has_this_parameter() const {
    if (num_arguments() == 0) return false;
    auto* arg = arguments().front();
    return arg && arg->is_this();
}

BindingDef::BindingDef(Facet const* facet, Scope* parent_scope,
                       std::string name, Type const* type_spec,
                       Value* initializer):
    DeclSymbol(SymbolType::BindingDef, facet, parent_scope, std::move(name),
               ScopeArg::None,
               /* num_generic_params: */ 0),
    _type_spec(type_spec),
    _init(initializer) {}

static std::string_view name_proj(Symbol const* symbol) {
    using namespace std::string_view_literals;
    return symbol ? symbol->name() : "NULL"sv;
}

static void verify_gen_inst(DeclSymbol const* def,
                            SubContext const& sub_context) {
    PRISM_ASSERT(sub_context.depth() == def->generic_nesting_depth() + 1);
    PRISM_ASSERT(sub_context.top_level().size() == def->num_generic_params());
}

static std::string make_gen_inst_name(DeclSymbol const* def,
                                      SubContext const& sub_context) {
    std::span generic_args = sub_context.level(def->generic_nesting_depth());
    if (generic_args.empty()) return def->name();
    std::stringstream sstr;
    sstr << def->name() << "(";
    print_separated(sstr, ", ", generic_args, name_proj);
    sstr << ")";
    return std::move(sstr).str();
}

StructInst::StructInst(StructDef* definition, SubContext const& sub_context):
    Type(SymbolType::StructInst, /* facet: */ nullptr,
         definition->parent_scope(),
         make_gen_inst_name(definition, sub_context), ScopeArg::None,
         // FIXME: compute correct layout here if possible
         TypeLayout::Incomplete),
    InstantiationBaseMixin(definition, sub_context) {
    set_flag(ExcludeFromNameLookup, true);
    verify_gen_inst(definition, sub_context);
}

TypeAliasInst::TypeAliasInst(TypeAliasDef* definition,
                             SubContext const& sub_context, Type* aliased):
    Type(SymbolType::TypeAliasInst, /* facet: */ nullptr,
         definition->parent_scope(),
         make_gen_inst_name(definition, sub_context), ScopeArg::None,
         aliased ? aliased->layout() : TypeLayout::Incomplete),
    InstantiationBaseMixin(definition, sub_context),
    _aliased(aliased) {
    set_flag(ExcludeFromNameLookup, true);
    verify_gen_inst(definition, sub_context);
}

static constexpr utl::streammanip SymbolName = [](std::ostream& str,
                                                  Symbol const* symbol) {
    if (symbol)
        str << symbol->name();
    else
        str << "NULL";
};

std::string make_name(Symbol const* bound, size_t index, size_t nesting_depth) {
    return utl::strcat("<gen-param.", nesting_depth, ".", index, ":",
                       SymbolName(bound), ">");
}

GenTypeParam::GenTypeParam(Trait const* trait_bound, size_t index,
                           size_t nesting_depth):
    Type(SymbolType::GenTypeParam, /* facet: */ nullptr,
         /* parent_scope: */ nullptr,
         make_name(trait_bound, index, nesting_depth), ScopeArg::None,
         TypeLayout::Incomplete),
    GenParamBase(index, nesting_depth),
    _trait_bound(trait_bound) {}

std::string FunctionType::make_name(FuncSig const& sig) {
    std::stringstream sstr;
    sstr << "fn (";
    print_separated(sstr, ", ", sig.arguments(), [](FuncArgSpec arg) {
        return utl::streammanip([=](std::ostream& str) {
            str << arg.passing_convention() << " " << SymbolName(arg.type());
        });
    });
    sstr << ") -> " << SymbolName(sig.return_type());
    return std::move(sstr).str();
}

TraitInst::TraitInst(TraitDef* definition, SubContext const& sub_context):
    Trait(SymbolType::TraitInst, /* facet: */ nullptr,
          definition->parent_scope(),
          make_gen_inst_name(definition, sub_context), ScopeArg::None),
    InstantiationBaseMixin(definition, sub_context) {
    set_flag(ExcludeFromNameLookup, true);
    verify_gen_inst(definition, sub_context);
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
             value.toString(), ScopeArg::None, type, Mutability::Const,
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

GenValueParam::GenValueParam(Type const* type, size_t index,
                             size_t nesting_depth):
    Value(SymbolType::GenValueParam, /* facet: */ nullptr,
          /* parent_scope: */ nullptr, make_name(type, index, nesting_depth),
          ScopeArg::None, type, Mutability::Const, ValueCat::LValue),
    GenParamBase(index, nesting_depth) {}

#if 0
static constexpr utl::streammanip FuncArgSpecProj = [](std::ostream& str,
                                                       FuncArgSpec arg) {
    str << arg.passing_convention() << " " << name_proj(arg.type());
};
#endif

FunctionInst::FunctionInst(FunctionDef* definition,
                           SubContext const& sub_context,
                           FunctionType const* type):
    Function(SymbolType::FunctionInst, /* facet: */ nullptr,
             definition->parent_scope(),
             make_gen_inst_name(definition, sub_context), type),
    InstantiationBaseMixin(definition, sub_context) {
    set_flag(ExcludeFromNameLookup, true);
    verify_gen_inst(definition, sub_context);
}

YieldInst const* BlockInst::get_yield_inst() const {
    if (_instructions.empty()) return nullptr;
    return dyncast<YieldInst const*>(_instructions.back());
}

YieldInst::YieldInst(SemaContext& ctx, Facet const* facet, Scope* parent_scope,
                     Value* operand):
    Instruction(SymbolType::YieldInst, facet, parent_scope, /* name: */ {},
                ScopeArg::None, ctx.get_void_type(), Mutability::Const,
                ValueCat::RValue, operand) {}
