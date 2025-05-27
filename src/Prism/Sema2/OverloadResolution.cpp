#include "Prism/Sema2/OverloadResolution.h"

#include <ostream>

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Facet/Facet.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/SemaDiagnostic.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::zip;

static bool deduce_generic_arg(size_t gen_nesting_depth,
                               Symbol const* param_sym, Symbol* arg_sym,
                               std::span<Symbol*> deduced_args) {
    if (!param_sym || !arg_sym) return false;
    if (param_sym == arg_sym) return true;
    if (auto* gen_param = dyncast<GenTypeParam const*>(param_sym);
        gen_param && gen_param->nesting_depth() == gen_nesting_depth)
    {
        size_t index = gen_param->index();
        if (deduced_args[index]) return deduced_args[index] == arg_sym;
        deduced_args[index] = arg_sym;
        return true;
    }
    if (auto* gen_param = dyncast<GenValueParam const*>(param_sym);
        gen_param && gen_param->nesting_depth() == gen_nesting_depth)
    {
        size_t index = gen_param->index();
        if (deduced_args[index]) return deduced_args[index] == arg_sym;
        deduced_args[index] = arg_sym;
        return true;
    }
    // For compound types, recur on inner structure
    // clang-format off
    return visit(*param_sym, *arg_sym, csp::overload{
        [&](StructInst const& param, StructInst const& arg) {
            if (param.definition() != arg.definition()) return false;
            for (auto [p, a]: zip(param.generic_args(), arg.generic_args()))
                if (!deduce_generic_arg(gen_nesting_depth, p, a, deduced_args))
                    return false;
            return true;
        },
        [](Symbol const&, Symbol const&) { return false; }
    }); // clang-format on
}

FunctionInst* prism::deduce_generic_function(
    SemaContext& ctx, FunctionDef* generic,
    std::span<Value const* const> arguments) {
    auto func_sig = generic->make_signature();
    if (func_sig.arguments().size() != arguments.size()) return nullptr;
    utl::small_vector<Symbol*> deduced_args(generic->generic_params().size());
    for (auto [param, arg]: zip(func_sig.arguments(), arguments)) {
        PRISM_ASSERT(arg);
        auto* arg_type = const_cast<Type*>(arg->type());
        if (!deduce_generic_arg(generic->generic_nesting_depth(), param.type(),
                                arg_type, deduced_args))
            return nullptr;
    }
    if (ranges::any_of(deduced_args, FN1(, _1 == nullptr))) return nullptr;
    return ctx.get_function_instantiation(generic, deduced_args);
}

// FIXME: remove this
static SourceContext const* get_source_context(Symbol const* sym) {
    if (!sym) return nullptr;
    auto* scope = sym->parent_scope();
    while (scope) {
        if (auto* sourceFile =
                dyncast<SourceFile const*>(scope->defining_symbol()))
            return &sourceFile->source_context();
        scope = scope->parent_scope();
    }
    return nullptr;
}

static std::pair<Facet const*, SourceContext const*> get_def_facet_and_ctx(
    Symbol const* function) {
    auto* def = [&] {
        if (auto* inst = dyncast<FunctionInst const*>(function))
            return inst->definition();
        return dyncast<FunctionDef const*>(function);
    }();
    if (!def) return { nullptr, nullptr };
    return { def->facet(), get_source_context(def) };
}

static std::unique_ptr<AmbiguousCall> make_ambi_err(
    SourceContext const* source_context, Facet const* call_facet,
    std::string name, std::span<Function const* const> candidates) {
    auto err =
        std::make_unique<AmbiguousCall>(source_context, call_facet, name);
    for (auto* candidate: candidates) {
        auto [facet, src_ctx] = get_def_facet_and_ctx(candidate);
        err->add_note(src_ctx, facet,
                      [=](std::ostream& str) { str << "possible candidate"; });
    }
    return err;
}

static std::unique_ptr<NoMatchingFunction> make_no_match_err(
    SourceContext const* source_context, Facet const* call_facet,
    std::string name, std::span<Symbol* const> overload_set) {
    auto err =
        std::make_unique<NoMatchingFunction>(source_context, call_facet, name);
    for (auto* function: overload_set) {
        auto [facet, src_ctx] = get_def_facet_and_ctx(function);
        err->add_note(src_ctx, facet,
                      [=](std::ostream& str) { str << "not a match"; });
    }
    return err;
}

ORResult prism::resolve_overload(SemaContext& ctx,
                                 SourceContext const* source_context,
                                 Facet const* call_facet, std::string name,
                                 std::span<Symbol* const> overload_set,
                                 std::span<Value const* const> arguments) {
    utl::small_vector<Function*> candidates;
    utl::small_vector<FunctionDef*> generics;
    for (auto* sym: overload_set) {
        if (auto* function = dyncast<Function*>(sym)) {
            bool match = ranges::equal(function->arguments(), arguments,
                                       ranges::equal_to{}, FN1(, _1.type()),
                                       FN1(, _1->type()));
            if (match) candidates.push_back(function);
        }
        else {
            generics.push_back(cast<FunctionDef*>(sym));
        }
    }
    if (candidates.size() == 1) return candidates.front();
    if (candidates.size() > 1)
        return utl::unexpected(
            make_ambi_err(source_context, call_facet, name, candidates));
    for (auto* generic: generics) {
        auto* function = deduce_generic_function(ctx, generic, arguments);
        if (function) candidates.push_back(function);
    }
    if (candidates.size() == 1) return candidates.front();
    if (candidates.size() > 1)
        return utl::unexpected(
            make_ambi_err(source_context, call_facet, name, candidates));
    return utl::unexpected(
        make_no_match_err(source_context, call_facet, name, overload_set));
}
