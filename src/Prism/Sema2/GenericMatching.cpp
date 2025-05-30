#include "Prism/Sema2/GenericMatching.h"

#include <range/v3/algorithm.hpp>
#include <utl/concepts.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema2/SemaContext.h"
#include "Prism/Sema2/Symbol.h"

using namespace prism;

using ranges::views::zip;

static auto do_match_generic(Symbol const* param_sym, Symbol const* arg_sym,
                             auto&& matcher) {
    if (!param_sym || !arg_sym) return matcher.null_fallback();
    if (param_sym == arg_sym) return matcher.exact_match();
    // For compound types, recur on inner structure
    // clang-format off
    return visit(*param_sym, *arg_sym, csp::overload{
        [&](std::derived_from<GenParamBase> auto const& gen_param,
            Symbol const& arg)
        {
            return matcher.gen_param(gen_param, arg);
        },
        [&](StructInst const& param, StructInst const& arg) {
            return matcher.structural(param, arg);
        },
        [&](Symbol const& param, Symbol const& arg) {
            return matcher.base_case(param, arg);
        }
    }); // clang-format on
}

bool prism::match_generic(
    Symbol const* param_sym, Symbol const* arg_sym,
    utl::function_view<bool(GenParamBase const&, Symbol const&)>
        compare_gen_param) {
    struct Matcher {
        bool null_fallback() const { return false; }
        bool exact_match() const { return true; }
        bool gen_param(GenParamBase const& gen_param,
                       Symbol const& arg_sym) const {
            return compare_gen_param(gen_param, arg_sym);
        }
        bool structural(StructInst const& param, StructInst const& arg) const {
            if (param.definition() != arg.definition()) return false;
            for (auto [p, a]: zip(param.generic_args(), arg.generic_args()))
                if (!do_match_generic(p, a, *this)) return false;
            return true;
        }
        bool base_case(Symbol const&, Symbol const&) const { return false; }
        decltype(compare_gen_param) compare_gen_param;
    };
    return do_match_generic(param_sym, arg_sym, Matcher{ compare_gen_param });
}

static int deduce_for_argument(SubContext& sub_context, Symbol const* param_sym,
                               Symbol* arg_sym) {
    struct Matcher {
        bool null_fallback() const { return false; }
        bool exact_match() {
            have_exact_match = true;
            return true;
        }
        bool gen_param(GenParamBase const& gen_param,
                       Symbol const& arg_sym) const {
            size_t index = gen_param.index();
            std::span level_list = sub_context.level(gen_param.nesting_depth());
            if (level_list[index]) return level_list[index] == &arg_sym;
            level_list[index] = const_cast<Symbol*>(&arg_sym);
            return true;
        }
        bool structural(StructInst const& param, StructInst const& arg) {
            if (param.definition() != arg.definition()) return false;
            for (auto [p, a]: zip(param.generic_args(), arg.generic_args())) {
                ++level;
                max_level = std::max(max_level, level);
                bool did_match = do_match_generic(p, a, *this);
                --level;
                if (!did_match) return false;
            }
            return true;
        }
        bool base_case(Symbol const&, Symbol const&) const { return false; }
        SubContext& sub_context;
        bool have_exact_match = false;
        int level = 1, max_level = 1;
    };
    Matcher matcher{ sub_context };
    if (!do_match_generic(param_sym, arg_sym, matcher)) return 0;
    if (matcher.have_exact_match && matcher.max_level == 1) return INT_MAX;
    return matcher.max_level;
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

GenericDeductionResult prism::deduce_generic_args(
    SubContext const& sub_context_in, FunctionDef& generic,
    std::span<Value const* const> call_args) {
    if (generic.num_arguments() != call_args.size())
        return { .success = false };
    GenericDeductionResult result{ .success = true,
                                   .sub_context = sub_context_in };
    prepare_sub_context(generic, result.sub_context);
    auto func_sig = generic.make_signature();
    for (auto [param, arg]: zip(func_sig.arguments(), call_args)) {
        PRISM_ASSERT(arg);
        auto* arg_type = const_cast<Type*>(arg->type());
        int score =
            deduce_for_argument(result.sub_context, param.type(), arg_type);
        if (score == 0) return { .success = false };
        result.score_vec.push_back(score);
    }
    if (result.sub_context.any_is_null()) return { .success = false };
    return result;
}
