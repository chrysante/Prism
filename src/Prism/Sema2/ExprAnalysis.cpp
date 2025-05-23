#include "Prism/Sema2/ExprAnalysis.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/scope_guard.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/Functional.h"
#include "Prism/Common/Ranges.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/AnalysisContext.h"
#include "Prism/Sema2/Scope.h"
#include "Prism/Sema2/SemaContext.h"
// #include "Prism/Sema2/SemaDiagnostic.h"
#include "Prism/Sema2/NameLookup.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using ranges::views::transform;
using ranges::views::zip;

namespace {

struct AnaContext: AnalysisContext {
    Scope* scope;

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

    Symbol* do_analyze(Facet const&) { PRISM_UNREACHABLE(); }
    Symbol* analyze_identifier(TerminalFacet const& id);
#if 0
    IntLiteral* analyze_int_literal(TerminalFacet const& term, int base);
#endif
    Symbol* do_analyze(TerminalFacet const& term);
    Symbol* do_analyze(BinaryFacet const& binary);
    Symbol* do_analyze_scope_resolution(BinaryFacet const& binary);
    Symbol* do_analyze(FnTypeFacet const& facet);
    Symbol* do_analyze(NamedParamDeclFacet const& declFacet);
    Symbol* do_analyze(PrefixFacet const& prefix);
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
    PRISM_UNIMPLEMENTED();
#if 0
    context.DE.emit<BadSymRef>(context.sourceContext, facet, symbol, expected);
#endif
}

Symbol* prism::analyze_facet(AnalysisContext const& context, Scope* scope,
                             Facet const* facet) {
    return AnaContext{ context, scope }.analyze(facet);
}

Symbol* AnaContext::analyze(Facet const* facet) {
    if (!facet) return nullptr;
    auto* sym = visit(*facet, FN1(&, do_analyze(_1)));
    if (auto* struct_def = dyncast<StructDef*>(sym))
        if (auto* canonical = struct_def->canonical()) return canonical;
    if (auto* trait_def = dyncast<TraitDef*>(sym))
        if (auto* canonical = trait_def->canonical()) return canonical;
    return sym;
}

Symbol* AnaContext::do_analyze(TerminalFacet const& term) {
    switch (term.token().kind) {
    case TokenKind::Type:
        return ctx.get_type_trait();
    case TokenKind::Identifier:
        return analyze_identifier(term);
#if 0
    case TokenKind::IntLiteralBin:
        return analyze_int_literal(term, 2);
    case TokenKind::IntLiteralDec:
        return analyze_int_literal(term, 10);
    case TokenKind::IntLiteralHex:
        return analyze_int_literal(term, 16);
#endif
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
    if (!LHS->scope()) PRISM_UNIMPLEMENTED(); // TODO: push error
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

template <typename T, typename... Args>
concept AnyOf = (std::same_as<T, Args> || ...);

Symbol* AnaContext::analyze_identifier(TerminalFacet const& id) {
    auto name = source_context->getTokenStr(id.token());
    auto symbols = unqualified_lookup(scope, name);
    // clang-format off
    using NLR = NameLookupResult;
    return symbols.visit(csp::overload{
        [&](AnyOf<NLR::None, NLR::Similar> auto) -> Symbol* {
            PRISM_UNIMPLEMENTED();
#if 0
            DE.emit<UndeclaredID>(source_context, &id, symbols.similar());
#endif
            return nullptr;
        },
        [&](Symbol* symbol) -> Symbol* { return symbol; },
        [&](std::span<Function* const> /* overload_set */) -> Symbol* {
            PRISM_UNIMPLEMENTED();
        },
        [&](std::span<Symbol const* const> ambi_set) -> Symbol* {
            PRISM_UNIMPLEMENTED();
#if 0
            DE.emit<AmbiguousNameLookup>(source_context, &id, ambi_set);
#endif
            return nullptr;
        },
    }); // clang-format on
}

#if 0
IntLiteral* AnaContext::analyze_int_literal(TerminalFacet const& term, int base) {
    PRISM_UNIMPLEMENTED();
    auto str = sourceContext->getTokenStr(term.token());
    auto value = APInt::parse(str, base, 32);
    if (!value) PRISM_UNIMPLEMENTED();
    return ctx.make<IntLiteral>(&term, *std::move(value), ctx.getInt32());
}
#endif

Symbol* AnaContext::do_analyze(PrefixFacet const& prefix) {
    auto* operand = analyze(prefix.operand());
    if (!operand) return nullptr;
    PRISM_UNIMPLEMENTED();
}

Symbol* AnaContext::do_analyze(CallFacet const& call) {
    auto* callee = analyze(call.callee());
    auto args = call.arguments()->elems() | transform(FN1(&, analyze(_1))) |
                ToSmallVector<>;
    if (!callee || !ranges::all_of(args, ToAddress)) return nullptr;
    if (auto* struct_def = dyncast<StructDef*>(callee))
        return ctx.get_struct_instantiation(struct_def, args);
    if (auto* trait_def = dyncast<TraitDef*>(callee))
        return ctx.get_trait_instantiation(trait_def, args);
    PRISM_UNIMPLEMENTED();
}
