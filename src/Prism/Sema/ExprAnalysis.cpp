#include "Prism/Sema/ExprAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema/AnalysisContext.h"
#include "Prism/Sema/GenericMatching.h"
#include "Prism/Sema/NameLookup.h"
#include "Prism/Sema/OverloadResolution.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Sema/SemaDiagnostic.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;
using ranges::views::zip;

namespace {

struct AnaContext: AnalysisContext {
    FacetAnalysisDelegate& delegate;
    SubContext& sub_context;
    Scope* scope;
    std::vector<Instruction*>* instructions = nullptr;

    SourceContext const* get_source_context(Facet const* facet) {
        if (source_context) return source_context;
        return source_context = prism::get_source_context(facet);
    }

    void emit_instruction(Instruction& inst) {
        delegate.emit_instruction(inst);
        if (instructions) instructions->push_back(&inst);
    }

    Symbol* analyze(Facet const* facet);

    template <std::derived_from<Symbol> S>
    S* analyze_as(Facet const* facet) {
        auto* symbol = analyze(facet);
        return verify_symbol_type<S>(symbol, facet);
    }

    template <std::derived_from<Symbol> S = Symbol, typename F = Facet>
    utl::small_vector<S*> analyze_list(std::span<F const* const> facets) {
        utl::small_vector<S*> result;
        for (auto* facet: facets)
            result.push_back(analyze_as<S>(facet));
        return result;
    }

    template <std::derived_from<Symbol> S>
    S* verify_symbol_type(Symbol* symbol, Facet const* facet) {
        return detail::verify_symbol_type<S>(*this, facet, symbol);
    }

    template <std::derived_from<Symbol> S>
    std::pair<utl::small_vector<S*>, bool> verify_list(
        std::span<Symbol* const> list, std::span<Facet const* const> facets) {
        PRISM_ASSERT(list.size() == facets.size());
        std::pair<utl::small_vector<S*>, bool> result{ {}, true };
        auto& [converted, success] = result;
        converted.reserve(list.size());
        for (auto [sym, facet]: zip(list, facets)) {
            if (auto* s = verify_symbol_type<S>(sym, facet))
                converted.push_back(s);
            else
                success = false;
        }
        return result;
    }

    Symbol* do_analyze(Facet const&) { PRISM_UNREACHABLE(); }
    Symbol* do_analyze(CompoundFacet const& facet);
    Symbol* do_analyze(VarDeclFacet const& var_decl_facet);
    Symbol* do_analyze(ExprStmtFacet const& stmt_facet);
    Symbol* analyze_this_literal();
    Symbol* analyze_identifier(TerminalFacet const& id);
    IntLiteral* analyze_int_literal(TerminalFacet const& term, int base);
    Symbol* do_analyze(TerminalFacet const& term);
    Symbol* do_analyze(BinaryFacet const& binary);
    Symbol* do_analyze_scope_resolution(BinaryFacet const& binary);
    Symbol* do_analyze(FnTypeFacet const& facet);
    Symbol* do_analyze(NamedParamDeclFacet const& declFacet);
    Symbol* determine_this_type();
    Symbol* do_analyze(PrefixFacet const& prefix);
    bool validate_generic_args(DeclSymbol const& decl, Facet const* call_facet,
                               std::span<Symbol* const> args,
                               std::span<Facet const* const> arg_facets);
    bool validate_call_arguments(Function const* callee,
                                 std::span<Symbol* const> args,
                                 std::span<Facet const* const> arg_facets);
    bool validate_num_call_arguments(Facet const* call_facet,
                                     Symbol const* callee, size_t num_params,
                                     size_t num_args);
    Symbol* do_analyze(CallFacet const& call);

    decltype(auto) with_scope(Scope* tempScope, std::invocable auto&& f) {
        PRISM_ASSERT(tempScope, "must not be null");
        auto stashed = std::exchange(scope, tempScope);
        utl::scope_guard pop = [&] { scope = stashed; };
        return std::invoke(f);
    }
};

} // namespace

template <ranges::range R>
    requires std::is_pointer_v<ranges::range_value_t<R>>
static bool is_any_null(R&& ptr_range) {
    return ranges::any_of(ptr_range, FN1(, _1 == nullptr));
}

void detail::push_bad_sym_ref(AnalysisContext const& context,
                              Facet const* facet, Symbol* symbol,
                              SymbolType expected) {
    context.DE.emit<BadSymRef>(facet, symbol, expected);
}

Symbol* prism::analyze_facet(AnalysisContext const& context,
                             FacetAnalysisDelegate& delegate,
                             SubContext& sub_context, Scope* scope,
                             Facet const* facet) {
    AnaContext facet_context{ context, delegate, sub_context, scope };
    return facet_context.analyze(facet);
}

Symbol* AnaContext::analyze(Facet const* facet) {
    if (!facet) return nullptr;
    auto* sym = visit(*facet, FN1(&, do_analyze(_1)));
    if (sym) delegate.encounter_callback(sym);
    return sym;
}

Symbol* AnaContext::do_analyze(CompoundFacet const& facet) {
    auto* outer_scope = scope;
    auto* outer_inst_list = instructions;
    scope = ctx.make_scope(outer_scope);
    std::vector<Instruction*> block_instructions;
    instructions = &block_instructions;
    for (auto* elem: facet.statements()->elems())
        analyze(elem);
    auto* block_type = [&]() -> Type const* {
        auto* yield_facet = facet.yieldFacet();
        if (!yield_facet) return ctx.get_void_type();
        auto* operand = analyze_as<Value>(yield_facet);
        if (!operand) return nullptr; // TODO: return poison type
        if (operand->type() == ctx.get_void_type()) return operand->type();
        auto* yield_inst = ctx.make<YieldInst>(yield_facet, scope, operand);
        emit_instruction(*yield_inst);
        return operand->type();
    }();
    auto* block_inst = ctx.make<BlockInst>(&facet, outer_scope,
                                           /* name: */ std::string{},
                                           ScopeArg(scope), block_type,
                                           std::move(block_instructions));
    scope = outer_scope;
    instructions = outer_inst_list;
    emit_instruction(*block_inst);
    return block_inst;
}

Symbol* AnaContext::do_analyze(VarDeclFacet const& var_decl_facet) {
    std::string name = get_name(var_decl_facet.name());
    Type const* type = analyze_as<Type>(var_decl_facet.typespec());
    Value* init = analyze_as<Value>(var_decl_facet.initExpr());
    if (!init && !var_decl_facet.assignFacet())
        DE.emit<BindingMissingInit>(&var_decl_facet, name);
    // Try to infer type or check type correctness
    if (init) {
        auto* init_type = init->type();
        if (!type)
            type = init_type;
        else if (init_type && type != init_type)
            DE.emit<BadOperandType>(var_decl_facet.initExpr(), init, init_type);
    }
    // TODO: Maybe declare variable of poison type here
    if (!type) return nullptr;
    using enum Mutability;
    auto mut = var_decl_facet.declarator().kind == TokenKind::Let ? Const : Mut;
    if (!check_redefinition(scope, var_decl_facet, name)) return nullptr;
    auto* binding = ctx.make<BindingInst>(&var_decl_facet, scope,
                                          std::move(name), type, mut, init);
    emit_instruction(*binding);
    return binding;
}

Symbol* AnaContext::do_analyze(ExprStmtFacet const& stmt_facet) {
    (void)analyze(stmt_facet.expr());
    return nullptr;
}

Symbol* AnaContext::do_analyze(TerminalFacet const& term) {
    switch (term.token().kind) {
    case TokenKind::Type:
        return ctx.get_type_trait();
    case TokenKind::Void:
        return ctx.get_void_type();
    case TokenKind::Byte:
        return ctx.get_byte_type();
    case TokenKind::Bool:
        return ctx.get_bool_type();
    case TokenKind::Int8:
        return ctx.get_i8_type();
    case TokenKind::Int16:
        return ctx.get_i16_type();
    case TokenKind::Int32:
        return ctx.get_i32_type();
    case TokenKind::Int64:
        return ctx.get_i64_type();
    case TokenKind::UInt8:
        return ctx.get_u8_type();
    case TokenKind::UInt16:
        return ctx.get_u16_type();
    case TokenKind::UInt32:
        return ctx.get_u32_type();
    case TokenKind::UInt64:
        return ctx.get_u64_type();
    case TokenKind::Float32:
        return ctx.get_f32_type();
    case TokenKind::Float64:
        return ctx.get_f64_type();
    case TokenKind::This:
        return analyze_this_literal();
    case TokenKind::Identifier:
        return analyze_identifier(term);
    case TokenKind::IntLiteralBin:
        return analyze_int_literal(term, 2);
    case TokenKind::IntLiteralDec:
        return analyze_int_literal(term, 10);
    case TokenKind::IntLiteralHex:
        return analyze_int_literal(term, 16);
    default:
        PRISM_UNREACHABLE();
    }
}

Symbol* AnaContext::do_analyze(BinaryFacet const& binary) {
    if (binary.operationFacet() && binary.operation().kind == TokenKind::Period)
        return do_analyze_scope_resolution(binary);
    auto* LHS = analyze(binary.LHS());
    auto* RHS = analyze(binary.RHS());
    if (!binary.operationFacet() || !LHS || !RHS) return nullptr;
    Token op = binary.operation();
    switch (op.kind) {
    default:
        PRISM_UNREACHABLE();
    }
}

Symbol* AnaContext::do_analyze_scope_resolution(BinaryFacet const& binary) {
    PRISM_EXPECT(binary.operationFacet() &&
                 binary.operation().kind == TokenKind::Period);
    auto* LHS = analyze(binary.LHS());
    if (!LHS) return nullptr;
    if (!LHS->scope()) PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
    return with_scope(LHS->scope(), FN0(&, analyze(binary.RHS())));
}

Symbol* AnaContext::do_analyze(FnTypeFacet const& facet) {
    auto* retType = analyze_as<Type>(facet.retType());
    if (!facet.paramList()) return nullptr;
    auto argTypes = analyze_list<Type const>(facet.paramList()->elems());
    if (!retType || is_any_null(argTypes)) return nullptr;
    PRISM_UNIMPLEMENTED();
#if 0
    return ctx.make<FunctionType>(&facet, nullptr, retType,
                                  std::move(argTypes));
#endif
}

Symbol* AnaContext::do_analyze(NamedParamDeclFacet const& declFacet) {
    if (isa<FnTypeFacet>(declFacet.parent()->parent()))
        return analyze_as<Type>(declFacet.typespec());
    PRISM_UNIMPLEMENTED();
}

Symbol* AnaContext::analyze_this_literal() {
    for (auto* curr_scope = scope; curr_scope;
         curr_scope = curr_scope->parent_scope())
    {
        auto* func_def = dyncast<FunctionDef*>(curr_scope->defining_symbol());
        if (!func_def) continue;
        if (func_def->num_arguments() == 0) {
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
            return nullptr;
        }
        auto* arg = func_def->arguments().front();
        if (!arg) return nullptr;
        if (!arg->is_this()) {
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
            return nullptr;
        }
        return arg;
    }
    PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
}

template <typename T, typename... Args>
concept AnyOf = (std::same_as<T, Args> || ...);

static Symbol* strip_non_generic_decl(Symbol* sym) {
    if (auto* decl = dyncast<DeclSymbol*>(sym))
        if (!decl->is_generic()) return decl->canonical();
    return sym;
}

Symbol* AnaContext::analyze_identifier(TerminalFacet const& id) {
    auto name = get_source_context(&id)->getTokenStr(id.token());
    auto symbols = unqualified_lookup(scope, name);
    // clang-format off
    using NLR = NameLookupResult;
    return symbols.visit(csp::overload{
        [&](AnyOf<NLR::None, NLR::Similar> auto) -> Symbol* {
            DE.emit<UndeclaredID>(&id, symbols.similar());
            return nullptr;
        },
        [&](Symbol* symbol) {
            auto* decl = dyncast<DeclSymbol*>(symbol);
            if (!decl || decl->is_generic())
                return symbol;
            delegate.encounter_callback(decl);
            return decl->canonical();
        },
        [&](NLR::OverloadSet& overload_set) -> Symbol* {
            for (auto& sym: overload_set)
                sym = strip_non_generic_decl(sym);
            return ctx.make<OverloadSet>(std::string(name),
                                         std::move(overload_set));
        },
        [&](NLR::AmbiSet const& ambi_set) -> Symbol* {
            DE.emit<AmbiguousNameLookup>(&id, ambi_set);
            return nullptr;
        },
    }); // clang-format on
}

IntLiteral* AnaContext::analyze_int_literal(TerminalFacet const& term,
                                            int base) {
    auto str = get_source_context(&term)->getTokenStr(term.token());
    auto value = APInt::parse(str, base, 32); // 32 for now
    if (!value) PRISM_UNIMPLEMENTED();        // TODO: emit diagnostic
    return ctx.get_int_literal(&term, *std::move(value), /* is_signed: */ true);
}

static bool is_token(Facet const* facet, TokenKind kind) {
    auto* term = dyncast<TerminalFacet const*>(facet);
    return term && term->token().kind == kind;
}

Symbol* AnaContext::determine_this_type() {
    for (auto* s = scope; s; s = s->parent_scope()) {
        auto* sym = s->defining_symbol();
        if (auto* struct_type = dyncast<StructDef*>(sym))
            return struct_type->canonical();
        if (auto* trait = dyncast<TraitDef*>(sym))
            return ctx.get_trait_this_type(trait->canonical());
        if (auto* impl = dyncast<TraitImplDef*>(sym)) return impl->type();
    }
    PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
    return nullptr;
}

Symbol* AnaContext::do_analyze(PrefixFacet const& prefix) {
    if (is_token(prefix.operationFacet(), TokenKind::This) &&
        is_token(prefix.operand(), TokenKind::Type))
        return determine_this_type();
    auto* operand = analyze(prefix.operand());
    if (!operand) return nullptr;
    PRISM_UNIMPLEMENTED();
}

bool AnaContext::validate_generic_args(
    DeclSymbol const& decl, Facet const* call_facet,
    std::span<Symbol* const> args, std::span<Facet const* const> arg_facets) {
    PRISM_ASSERT(args.size() == arg_facets.size());
    if (decl.generic_params().size() != args.size()) {
        DE.emit<InvalidNumOfGenArgs>(call_facet, &decl, args.size());
        return false;
    }
    bool success = true;
    for (auto [param, arg, arg_facet]:
         zip(decl.generic_params(), args, arg_facets))
    {
        if (isa<Type>(param))
            success &= !!verify_symbol_type<Type>(arg, arg_facet);
        else if (isa<Value>(param))
            success &= !!verify_symbol_type<Value>(arg, arg_facet);
    }
    return success;
}

bool AnaContext::validate_call_arguments(
    Function const* callee, std::span<Symbol* const> args,
    std::span<Facet const* const> arg_facets) {
    PRISM_ASSERT(callee);
    PRISM_ASSERT(args.size() == arg_facets.size());
    PRISM_ASSERT(args.size() == callee->num_arguments());
    bool success = true;
    for (auto [arg_spec, arg_sym, arg_facet]:
         zip(callee->arguments(), args, arg_facets))
    {
        auto* arg = verify_symbol_type<Value>(arg_sym, arg_facet);
        if (!arg) {
            success = false;
            continue;
        }
        if (arg_spec.type() != arg->type()) {
            DE.emit<BadOperandType>(arg_facet, arg, arg_spec.type());
            success = false;
            continue;
        }
        switch (arg_spec.passing_convention()) {
        case PassingConvention::In:
            break;
        case PassingConvention::Inout:
            if (arg->is_const()) PRISM_UNIMPLEMENTED();
            break;
        case PassingConvention::Sink:
            break;
        }
    }
    return success;
}

bool AnaContext::validate_num_call_arguments(Facet const* call_facet,
                                             Symbol const* callee,
                                             size_t num_params,
                                             size_t num_args) {
    if (num_params == num_args) return true;
    DE.emit<InvalidNumOfCallArgs>(call_facet, callee, num_params, num_args);
    return false;
}

Symbol* AnaContext::do_analyze(CallFacet const& call_facet) {
    auto* callee = analyze(call_facet.callee());
    auto arg_facets = call_facet.arguments()->elems();
    auto args = arg_facets | transform(FN1(&, analyze(_1))) | ToSmallVector<>;
    if (!callee || !ranges::all_of(args, ToAddress)) return nullptr;
    if (auto* struct_def = dyncast<StructDef*>(callee)) {
        if (!validate_generic_args(*struct_def, &call_facet, args, arg_facets))
            return nullptr;
        auto sub_context_copy = sub_context;
        sub_context_copy.pop_to(struct_def->generic_nesting_depth());
        sub_context_copy.push(args);
        auto* struct_inst =
            ctx.get_struct_instantiation(sub_context_copy, struct_def);
        return struct_inst;
    }
    if (auto* trait_def = dyncast<TraitDef*>(callee)) {
        if (!validate_generic_args(*trait_def, &call_facet, args, arg_facets))
            return nullptr;
        auto sub_context_copy = sub_context;
        sub_context_copy.pop_to(trait_def->generic_nesting_depth());
        sub_context_copy.push(args);
        auto* trait_inst =
            ctx.get_trait_instantiation(sub_context_copy, trait_def);
        return trait_inst;
    }
    if (auto* function = dyncast<Function*>(callee)) {
        if (!validate_num_call_arguments(&call_facet, function,
                                         function->num_arguments(),
                                         args.size()))
            return nullptr;
        if (!validate_call_arguments(function, args, arg_facets))
            return nullptr;
        auto value_args = args | transform(cast<Value*>) | ToSmallVector<>;
        auto* call_inst = ctx.make<CallInst>(&call_facet, scope,
                                             /* name: */ std::string{},
                                             function, value_args);
        emit_instruction(*call_inst);
        return call_inst;
    }
    if (auto* generic = dyncast<FunctionDef*>(callee)) {
        if (!validate_num_call_arguments(&call_facet, generic,
                                         generic->num_arguments(), args.size()))
            return nullptr;
        auto [value_args, success] = verify_list<Value>(args, arg_facets);
        if (!success) return nullptr;
        auto deduction_result =
            deduce_generic_args(sub_context, *generic, value_args);
        if (!deduction_result.success) {
            PRISM_UNIMPLEMENTED(); // TODO: emit diagnostic
            return nullptr;
        }
        auto* function =
            ctx.get_function_instantiation(deduction_result.sub_context,
                                           generic);
        auto* call_inst = ctx.make<CallInst>(&call_facet, scope,
                                             /* name: */ std::string{},
                                             function, value_args);
        emit_instruction(*call_inst);
        return call_inst;
    }
    if (auto* overload_set = dyncast<OverloadSet*>(callee)) {
        auto [value_args, success] = verify_list<Value>(args, arg_facets);
        if (!success) return nullptr;
        auto overload_resultion_result =
            resolve_overload(ctx, sub_context, &call_facet,
                             overload_set->name(), overload_set->symbols(),
                             value_args);
        if (!overload_resultion_result) {
            DE.emit(std::move(overload_resultion_result).error());
            return nullptr;
        }
        auto* call_inst = ctx.make<CallInst>(&call_facet, scope,
                                             /* name: */ std::string{},
                                             *overload_resultion_result,
                                             value_args);
        emit_instruction(*call_inst);
        return call_inst;
    }
    DE.emit<SymbolNotCallable>(call_facet.callee(), callee);
    return nullptr;
}
