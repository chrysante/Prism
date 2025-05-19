#include "Prism/Sema/Contracts.h"

#include <range/v3/algorithm.hpp>

#include "Prism/Common/SyntaxMacros.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

void Obligation::addConformance(Symbol* sym, SpecAddMode mode) {
    switch (mode) {
    case SpecAddMode::Define:
        _conf = { sym };
        return;
    case SpecAddMode::Inherit:
        if (!ranges::contains(_conf, sym)) _conf.push_back(sym);
        return;
    }
}

TypeObligation::TypeObligation(Typedef* type, Symbol* owner):
    Obligation(SpecType::TypeObligation, type, owner) {}

Typedef* TypeObligation::type() const { return cast<Typedef*>(symbol()); }

FuncObligation::FuncObligation(Function* func, Symbol* owner):
    Obligation(SpecType::FuncObligation, func, owner) {}

Function* FuncObligation::function() const { return cast<Function*>(symbol()); }

static Type const* mapTypeParam(GenericTypeParam const* param,
                                Symbol const& def) {
    if (auto* traitImpl = dyncast<TraitImplDef const*>(&def)) {
        auto* inst = dyncast<GenTraitInst const*>(traitImpl->trait());
        if (!inst) return nullptr;
        auto genParams = inst->genTemplate()->genParams();
        auto itr = ranges::find(genParams, param);
        if (itr == genParams.end()) return nullptr;
        size_t index = utl::narrow_cast<size_t>(itr - genParams.begin());
        return dyncast<Type const*>(inst->genArguments()[index]);
    }
    PRISM_UNIMPLEMENTED();
}

struct detail::InterfaceCompareImpl {
    static bool impl(Type const* lhs, Type const* rhs,
                     InterfaceLike const& interface) {
        return lhs == rhs || implAsym(lhs, rhs, interface) ||
               implAsym(rhs, lhs, interface);
    }

    static bool implAsym(Type const* lhs, Type const* rhs,
                         InterfaceLike const& interface) {
        if (auto itr = interface._typedefDefinitionMap.find(lhs);
            itr != interface._typedefDefinitionMap.end())
        {
            for (auto* def: itr->second)
                if (impl(def, rhs, interface)) return true;
        }
        if (auto* lhsTypedef = dyncast<Typedef const*>(lhs)) {
            auto itr = interface._typedefOblMap.find(lhsTypedef);
            if (itr == interface._typedefOblMap.end())
                return impl(lhsTypedef->definition(), rhs, interface);
            for (auto* obl: itr->second)
                if (impl(obl->type(), rhs, interface)) return true;
        }
        return false;
    }
};

static auto makeTypeCmp(InterfaceLike const& interface) {
    return [&interface](Type const* lhs, Type const* rhs) -> bool {
        return detail::InterfaceCompareImpl::impl(lhs, rhs, interface);
    };
}

bool FuncObligationKey::Equal::operator()(FuncObligationKey const& lhs,
                                          FuncObligationKey const& rhs) const {
    return lhs.name == rhs.name &&
           lhs.funcSig.compareEqIgnoringFirst(rhs.funcSig,
                                              makeTypeCmp(*interface));
}

size_t FuncObligationKey::Hash::operator()(FuncObligationKey const& key) const {
    return std::hash<std::string_view>{}(key.name);
}

InterfaceLike::InterfaceLike(Symbol* symbol):
    _symbol(symbol), _funcObls(0, this, this) {}

InterfaceLike::~InterfaceLike() = default;

void InterfaceLike::addObligation(csp::unique_ptr<Obligation> obl,
                                  SpecAddMode mode) {
    if (!obl) return;
    if (visit(*obl, FN1(&, addObligationImpl(&_1, mode))))
        bag.push_back(std::move(obl));
}

void InterfaceLike::addTypeConformance(Typedef const* impl,
                                       TypeObligation const* obl) {
    _typedefOblMap[impl].push_back(obl);
    if (impl->definition())
        _typedefDefinitionMap[impl->definition()].push_back(impl);
}

bool InterfaceLike::addObligationImpl(TypeObligation* obl, SpecAddMode mode) {
    auto* type = obl->type();
    auto& list = _typeObls[type->name()];
    switch (mode) {
    case SpecAddMode::Define: {
        list = { obl };
        return true;
    }
    case SpecAddMode::Inherit:
        if (auto itr = ranges::find(list, type, FN1(_1->type()));
            itr != list.end())
        {
            auto* existing = *itr;
            for (auto* conf: obl->conformances())
                existing->addConformance(conf, SpecAddMode::Inherit);
            return false;
        }
        list.push_back(obl);
        return true;
    }
}

bool InterfaceLike::addObligationImpl(FuncObligation* obl, SpecAddMode mode) {
    auto* F = obl->function();
    auto& list = _funcObls[{ F->name(), F->signature() }];
    switch (mode) {
    case SpecAddMode::Define:
        list = { obl };
        return true;
    case SpecAddMode::Inherit:
        if (auto itr = ranges::find(list, F, FN1(_1->function()));
            itr != list.end())
        {
            auto* existing = *itr;
            for (auto* conf: obl->conformances())
                existing->addConformance(conf, SpecAddMode::Inherit);
            return false;
        }
        list.push_back(obl);
        return true;
    }
}

static bool isCompleteImpl(auto& obls, auto filter) {
    for (auto& [key, list]: obls)
        for (auto* obl: list)
            if (filter(obl) && !obl->singleConformance()) return false;
    return true;
}

bool InterfaceLike::isComplete() const {
    auto filter = FN1(true);
    return isCompleteImpl(_typeObls, filter) &&
           isCompleteImpl(_funcObls, filter);
}

bool InterfaceLike::isCompleteForTraits() const {
    auto filter = FN1(isa<Trait>(_1->owner()));
    return isCompleteImpl(_typeObls, filter) &&
           isCompleteImpl(_funcObls, filter);
}
