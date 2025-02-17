#include "Prism/Sema/Symbol.h"

#include <utl/strcat.hpp>

#include "Prism/Facet/Facet.h"
#include "Prism/Sema/Contracts.h"
#include "Prism/Sema/Scope.h"
#include "Prism/Sema/SemaContext.h"

using namespace prism;

Symbol::Symbol(SymbolType type, std::string name, Facet const* facet,
               Scope* parent):
    _symType(type), _name(std::move(name)), _facet(facet), _parent(parent) {
    if (parent) parent->addSymbol(*this);
}

Scope const* Symbol::associatedScope() const {
    return visit<Scope const*>(*this, []<typename S>(S const& sym) {
        if constexpr (std::is_base_of_v<detail::AssocScope, S>)
            return sym.detail::AssocScope::associatedScope();
        else
            return nullptr;
    });
}

detail::AssocScope::AssocScope(SemaContext& ctx, Scope* scope, Symbol* This):
    _scope(scope) {
    if (!_scope) _scope = ctx.makeScope(This->parentScope());
    _scope->_assocSymbol = This;
}

Module::Module(SymbolType type, SemaContext& ctx, std::string name,
               Facet const* facet, Scope* parent):
    Symbol(type, std::move(name), facet, parent),
    AssocScope(ctx, nullptr, this) {}

SourceFile::SourceFile(SemaContext& ctx, std::string name, Facet const* facet,
                       Scope* parent, SourceContext const& sourceCtx):
    Symbol(SymbolType::SourceFile, std::move(name), facet, parent),
    AssocScope(ctx, nullptr, this),
    sourceCtx(sourceCtx) {}

ScopedType::ScopedType(SymbolType symType, SemaContext& ctx, std::string name,
                       Facet const* facet, Scope* parent, Scope* scope,
                       TypeLayout layout):
    ValueType(symType, std::move(name), facet, parent, layout),
    AssocScope(ctx, scope, this) {}

TraitImplInterface const* TraitConformer::findTraitImpl(
    Trait const* trait) const {
    auto itr = _traitImpls.find(trait);
    return itr != _traitImpls.end() ? itr->second : nullptr;
}

/// \overload
std::span<GenTraitImpl const* const> TraitConformer::findTraitImpls(
    GenTrait const* trait) const {
    auto itr = _genTraitImpls.find(trait);
    if (itr != _genTraitImpls.end()) return itr->second;
    return {};
}

void TraitConformer::setTraitImpl(TraitImpl& impl) {
    auto [itr, success] = _traitImpls.insert({ impl.trait(), &impl });
    PRISM_ASSERT(success, "Duplicate implementation");
}

void TraitConformer::setTraitImpl(GenTraitImpl& impl) {
    if (auto* trait = dyncast<GenTraitInst*>(impl.trait())) {
        _genTraitImpls[trait->genTemplate()].push_back(&impl);
        return;
    }
    auto [itr, success] = _traitImpls.insert({ impl.trait(), &impl });
    PRISM_ASSERT(success, "Duplicate implementation");
}

GenStructTypeInst::GenStructTypeInst(SemaContext& ctx,
                                     GenStructType* typeTemplate,
                                     utl::small_vector<Symbol*>&& args):
    CompositeType(SymbolType::GenStructTypeInst, ctx, typeTemplate->name(),
                  /* facet: */ nullptr, typeTemplate->parentScope(),
                  TypeLayout::Incomplete),
    GenericInstantiation(typeTemplate, std::move(args)) {}

FuncInterface::FuncInterface(Symbol* function,
                             utl::small_vector<FuncParam*>&& params,
                             Type const* retType):
    _func(*function),
    _params(std::move(params)),
    _sig(FuncSig::Compute(retType, this->params())) {}

Function::Function(std::string name, Facet const* facet, Scope* parent,
                   utl::small_vector<FuncParam*>&& params, Type const* retType):
    Symbol(SymbolType::Function, std::move(name), facet, parent),
    FuncInterface(this, std::move(params), retType) {}

Function::Function(std::string name, Facet const* facet, Scope* parent):
    Symbol(SymbolType::Function, std::move(name), facet, parent),
    FuncInterface(this) {}

FunctionImpl::FunctionImpl(SemaContext& ctx, std::string name,
                           Facet const* facet, Scope* parent,
                           utl::small_vector<FuncParam*>&& params,
                           Type const* retType):
    Function(std::move(name), facet, parent, std::move(params), retType),
    AssocScope(ctx, nullptr, this) {
    setSymbolType(SymbolType::FunctionImpl);
}

GenFuncImpl::GenFuncImpl(SemaContext& ctx, std::string name, Facet const* facet,
                         Scope* parent, Scope* scope,
                         utl::small_vector<Symbol*>&& genParams,
                         utl::small_vector<FuncParam*>&& params,
                         Type const* retType):
    GenericSymbol(SymbolType::GenFuncImpl, ctx, std::move(name), facet, parent,
                  scope, std::move(genParams)),
    FuncInterface(this, std::move(params), retType) {}

GenTraitInst::GenTraitInst(SemaContext& ctx, GenTrait* traitTemplate,
                           utl::small_vector<Symbol*>&& arguments):
    Trait(SymbolType::GenTraitInst, ctx, traitTemplate->name(),
          traitTemplate->facet(), traitTemplate->parentScope()),
    GenericInstantiation(traitTemplate, std::move(arguments)) {}

GenTraitImplInst::GenTraitImplInst(SemaContext& ctx, GenTraitImpl* implTemplate,
                                   utl::small_vector<Symbol*>&& arguments,
                                   Trait* trait, CompositeType* conforming):
    TraitImpl(SymbolType::GenTraitImplInst, ctx, implTemplate->facet(),
              implTemplate->parentScope(), trait, conforming),
    GenericInstantiation(implTemplate, std::move(arguments)) {}

static std::string valueAsStrImpl(APInt const& value, IntType const* type,
                                  int base = 10) {
    if (type && type->isSigned())
        return value.signedToString(base);
    else
        return value.toString(base);
}

IntLiteral::IntLiteral(Facet const* facet, APInt value, IntType const* type):
    LiteralValue(SymbolType::IntLiteral,
                 utl::strcat("int-lit: ", valueAsStrImpl(value, type)), facet,
                 /* parent-scope: */ nullptr, QualType::Const(type), RValue),
    _value(std::move(value)) {}

std::string IntLiteral::valueAsString(int base) const {
    return valueAsStrImpl(value(), cast<IntType const*>(type().get()), base);
}

RetInst::RetInst(Scope* parent, Facet const* facet, Value* retval):
    Instruction(SymbolType::RetInst, /* name: */ {}, facet, parent, {},
                ValueCat::RValue, { retval }) {}
