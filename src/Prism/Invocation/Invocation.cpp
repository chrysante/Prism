#include "Prism/Invocation/Invocation.h"

#include <utl/hashtable.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Diagnostic/DiagnosticHandler.h"
#include "Prism/Parser/Parser.h"
#include "Prism/Sema/Analysis.h"
#include "Prism/Sema/SemaContext.h"
#include "Prism/Source/SourceContext.h"

using namespace prism;
using detail::InvImpl;

namespace {

class Bag {
public:
    template <typename T, typename U = std::remove_cvref_t<T>>
    U& add(T&& elem) {
        auto* p = new T(std::forward<T>(elem));
        elems.emplace_back(p, Delete<U>);
        return *p;
    }

private:
    template <typename T>
    static void Delete(void* p) {
        delete static_cast<T*>(p);
    }

    using UniquePtr = std::unique_ptr<void, void (*)(void*)>;
    std::vector<UniquePtr> elems;
};

} // namespace

struct detail::InvImpl {
    template <typename T>
    auto& retain(T&& arg) {
        return bag.add(std::forward<T>(arg));
    }

    Bag bag;
    MonotonicBufferResource resource;
    DiagnosticHandler diagHandler;
    std::vector<SourceContext> sources;
    utl::hashmap<std::filesystem::path, SourceFileFacet const*> parseTrees;
    SemaContext semaContext;
    Target* target = nullptr;
};

Invocation::Invocation(): impl(std::make_unique<InvImpl>()) {}

Invocation::Invocation(Invocation&&) noexcept = default;

Invocation& Invocation::operator=(Invocation&&) noexcept = default;

Invocation::~Invocation() = default;

void Invocation::addSourceFile(std::filesystem::path) { PRISM_UNIMPLEMENTED(); }

void Invocation::addSourceFile(std::filesystem::path path,
                               std::string sourceStr) {
    std::string_view source = impl->retain(std::move(sourceStr));
    impl->sources.emplace_back(std::move(path), source);
}

void Invocation::run() { runUntil(InvocationStage::Sema); }

static bool operator<(InvocationStage a, InvocationStage b) {
    return (int)a < (int)b;
}

void Invocation::runUntil(InvocationStage stage) {
    std::vector<SourceFilePair> sourceFilePairs;
    sourceFilePairs.reserve(impl->sources.size());
    for (auto& sourceContext: impl->sources) {
        auto* parseTree =
            parseSourceFile(impl->resource, sourceContext, impl->diagHandler);
        impl->parseTrees.insert({ sourceContext.filepath(), parseTree });
        sourceFilePairs.push_back({ parseTree, &sourceContext });
    }
    if (stage < InvocationStage::Sema) return;
    impl->target = analyzeModule(impl->resource, impl->semaContext,
                                 impl->diagHandler, sourceFilePairs);
}

DiagnosticHandler const& Invocation::getDiagnosticHandler() const {
    return impl->diagHandler;
}

SemaContext& Invocation::getSemaContext() { return impl->semaContext; }

SourceFileFacet const* Invocation::getParseTree(
    std::filesystem::path const& filepath) const {
    auto itr = impl->parseTrees.find(filepath);
    return itr != impl->parseTrees.end() ? itr->second : nullptr;
}

Target* Invocation::getTarget() const { return impl->target; }
