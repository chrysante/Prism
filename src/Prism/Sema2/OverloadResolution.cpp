#include "Prism/Sema2/OverloadResolution.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/SemaContext.h"
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

Function* prism::resolve_overload(SemaContext& ctx,
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
    for (auto* generic: generics) {
        auto* function = deduce_generic_function(ctx, generic, arguments);
        if (function) candidates.push_back(function);
    }
    if (candidates.size() == 1) return candidates.front();
    return nullptr;
}
