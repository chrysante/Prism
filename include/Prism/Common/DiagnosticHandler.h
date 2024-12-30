#ifndef PRISM_COMMON_DIAGNOSTICHANDLER_H
#define PRISM_COMMON_DIAGNOSTICHANDLER_H

#include <concepts>
#include <iosfwd>
#include <memory>
#include <ranges>
#include <vector>

#include <Prism/Common/Diagnostic.h>
#include <Prism/Common/Functional.h>

namespace prism {

class SourceContext;

/// TODO: Separate the interfaces for consumers and users of issues
class DiagnosticHandler {
    auto view() const { return list | std::views::transform(Dereference); }

public:
    template <std::derived_from<Diagnostic> I, typename... Args>
        requires std::constructible_from<I, Args&&...>
    I* push(Args&&... args) {
        auto* p = push(std::make_unique<I>(std::forward<Args>(args)...));
        return static_cast<I*>(p);
    }

    Diagnostic* push(std::unique_ptr<Diagnostic> issue) {
        list.push_back(std::move(issue));
        return list.back().get();
    }

    bool empty() const { return list.empty(); }

    size_t size() const { return list.size(); }

    void clear() { list.clear(); }

    auto begin() const { return view().begin(); }
    auto end() const { return view().end(); }
    auto const& front() const { return view().front(); }
    auto const& back() const { return view().back(); }

    /// Writes all issues to `std::cerr`
    void print(SourceContext const& ctx);

    /// Writes all issues formatted to \p os
    void format(std::ostream& os, SourceContext const& ctx) const;

private:
    std::vector<std::unique_ptr<Diagnostic>> list;
};

} // namespace prism

#endif // PRISM_COMMON_DIAGNOSTICHANDLER_H
