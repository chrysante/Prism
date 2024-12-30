#include <filesystem>
#include <memory>
#include <string>

namespace prism {

class SourceFileFacet;
class Target;
class IssueHandler;

namespace detail {
struct InvImpl;
}

enum class InvocationStage { Parser, Sema };

///
class Invocation {
public:
    Invocation();
    Invocation(Invocation&&) noexcept;
    Invocation& operator=(Invocation&&) noexcept;
    ~Invocation();

    /// MARK: - Setup

    /// Opens the file at \p path and adds it to the list of compiled files.
    /// \Throws if the file cannot be opened
    void addSourceFile(std::filesystem::path path);

    /// Adds the string \p source as a source file and pretends its location is
    /// \p path
    void addSourceFile(std::filesystem::path path, std::string source);

    /// MARK: - Execution

    /// Executes the compiler
    void run();

    /// Executes the compiler until including \p stage
    void runUntil(InvocationStage stage);

    /// MARK: - Retrieval

    ///
    IssueHandler const& getIssueHandler() const;

    /// \Returns the parse tree of the source file \p filepath
    SourceFileFacet const* getParseTree(
        std::filesystem::path const& filepath) const;

    /// \Returns the semanically analyzed sema target
    Target* getTarget() const;

private:
    std::unique_ptr<detail::InvImpl> impl;
};

} // namespace prism