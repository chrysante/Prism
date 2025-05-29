#include "Prism/Sema2/GenericMatching.h"

#include <range/v3/algorithm.hpp>
#include <range/v3/view.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/SubContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::zip;

static bool deduce_for_argument(SubContext& sub_context,
                                Symbol const* param_sym, Symbol* arg_sym) {
    if (!param_sym || !arg_sym) return false;
    if (param_sym == arg_sym) return true;
    if (auto* gen_param = as_gen_param_base(param_sym)) {
        size_t index = gen_param->index();
        std::span level_list = sub_context.level(gen_param->nesting_depth());
        if (level_list[index]) return level_list[index] == arg_sym;
        level_list[index] = arg_sym;
        return true;
    }
    // For compound types, recur on inner structure
    // clang-format off
    return visit(*param_sym, *arg_sym, csp::overload{
        [&](StructInst const& param, StructInst const& arg) {
            if (param.definition() != arg.definition()) return false;
            for (auto [p, a]: zip(param.generic_args(), arg.generic_args()))
                if (!deduce_for_argument(sub_context, p, a))
                    return false;
            return true;
        },
        [](Symbol const&, Symbol const&) { return false; }
    }); // clang-format on
}

static void prepare_sub_context(FunctionDef const& generic,
                                SubContext& sub_context) {
    PRISM_ASSERT_AUDIT(sub_context.none_is_null());
    size_t target_depth = generic.generic_nesting_depth() + 1;
    while (sub_context.depth() >= target_depth)
        sub_context.pop();
    utl::stack<DeclSymbol const*> generic_stack;
    size_t diff = target_depth - sub_context.depth();
    Symbol const* sym_itr = &generic;
    for (size_t i = 0; i < diff; ++i) {
        generic_stack.push(cast<DeclSymbol const*>(sym_itr));
        sym_itr = sym_itr->parent_scope()->defining_symbol();
    }
    while (!generic_stack.empty())
        sub_context.push_empty(generic_stack.pop()->num_generic_params());
}

std::optional<SubContext> prism::deduce_generic_args(
    SubContext const& sub_context_in, FunctionDef& generic,
    std::span<Value const* const> call_args) {
    if (generic.num_arguments() != call_args.size()) return std::nullopt;
    std::optional<SubContext> sub_context = sub_context_in;
    prepare_sub_context(generic, *sub_context);
    auto func_sig = generic.make_signature();
    for (auto [param, arg]: zip(func_sig.arguments(), call_args)) {
        PRISM_ASSERT(arg);
        auto* arg_type = const_cast<Type*>(arg->type());
        if (!deduce_for_argument(*sub_context, param.type(), arg_type))
            return std::nullopt;
    }
    if (sub_context->any_is_null()) return std::nullopt;
    return sub_context;
}
