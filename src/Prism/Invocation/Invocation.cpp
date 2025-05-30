#include "Prism/Invocation/Invocation.h"

#include <cstring>
#include <fstream>
#include <sstream>

#include <utl/hashtable.hpp>
#include <utl/strcat.hpp>

#include "Prism/Common/Assert.h"
#include "Prism/Diagnostic/DiagnosticEmitter.h"
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

    InvImpl(): DE(makeDefaultDiagnosticEmitter()) {}

    Bag bag;
    MonotonicBufferResource resource;
    std::unique_ptr<DiagnosticEmitter> DE;
    std::vector<SourceContext> sources;
    utl::hashmap<std::filesystem::path, SourceFileFacet const*> parse_trees;
    SemaContext sema_context;
    Module* target = nullptr;
};

Invocation::Invocation(): impl(std::make_unique<InvImpl>()) {}

Invocation::Invocation(Invocation&&) noexcept = default;

Invocation& Invocation::operator=(Invocation&&) noexcept = default;

Invocation::~Invocation() = default;

[[noreturn]]
static void throwFileError(std::filesystem::path const& path, int err) {
    std::stringstream sstr;
    sstr << "Failed to open file " << path << ": " << strerror(err);
    throw std::runtime_error(std::move(sstr).str());
}

void Invocation::add_source_file(std::filesystem::path path) {
    std::fstream file(path);
    if (!file) throwFileError(path, errno);
    std::stringstream sstr;
    sstr << file.rdbuf();
    add_source_file(std::move(path), std::move(sstr).str());
}

void Invocation::add_source_file(std::filesystem::path path,
                                 std::string sourceStr) {
    std::string_view source = impl->retain(std::move(sourceStr));
    impl->sources.emplace_back(std::move(path), source);
}

void Invocation::run() { run_until(InvocationStage::Sema); }

static bool operator<(InvocationStage a, InvocationStage b) {
    return (int)a < (int)b;
}

void Invocation::run_until(InvocationStage stage) {
    std::vector<SourceFilePair> source_file_pairs;
    source_file_pairs.reserve(impl->sources.size());
    for (auto& sourceContext: impl->sources) {
        auto* parseTree =
            parseSourceFile(impl->resource, sourceContext, *impl->DE);
        impl->parse_trees.insert({ sourceContext.filepath(), parseTree });
        source_file_pairs.push_back({ parseTree, &sourceContext });
    }
    if (stage < InvocationStage::Sema) return;
    // For now we return before sema if we have parsing errors
    if (!impl->DE->empty()) return;
    impl->target =
        analyze_module(impl->sema_context, *impl->DE, source_file_pairs);
}

DiagnosticEmitter const& Invocation::get_diagnostic_emitter() const {
    return *impl->DE;
}

SemaContext& Invocation::get_sema_context() { return impl->sema_context; }

SourceFileFacet const* Invocation::get_parse_tree(
    std::filesystem::path const& filepath) const {
    auto itr = impl->parse_trees.find(filepath);
    return itr != impl->parse_trees.end() ? itr->second : nullptr;
}

Module* Invocation::get_module() const { return impl->target; }
