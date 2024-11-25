#ifndef PRISM_SOURCE_SOURCECONTEXT_H
#define PRISM_SOURCE_SOURCECONTEXT_H

#include <cstdint>
#include <filesystem>
#include <string_view>
#include <utility>
#include <vector>

#include <Prism/Source/SourceLocation.h>
#include <Prism/Source/Token.h>

namespace prism {

/// Maps indices into the source text to source locations, i.e. line and column
/// information.
///
/// This allows us to only store tokens as indices and length and only lookup
/// the extended source location if needed
class SourceLocationMap {
public:
    /// Constructs the source location map
    static SourceLocationMap Build(std::string_view source);

    SourceLocationMap() = default;

    /// \Returns the source location at \p index
    SourceLocation operator[](uint32_t index) const;

private:
    std::vector<std::pair<uint32_t, uint32_t>> map;
};

/// Wraps metadata and source text for a given source file
class SourceContext {
public:
    explicit SourceContext(std::filesystem::path filepath,
                           std::string_view source):
        p(std::move(filepath)), src(source) {}

    SourceLocation getSourceLocation(uint32_t index) const {
        buildSourceLocationMapLazy();
        return (*locMap)[index];
    }

    std::filesystem::path const& filepath() const { return p; }

    std::string_view source() const { return src; }

    std::string_view getTokenStr(Token tok) const {
        return src.substr(tok.index, tok.sourceLen);
    }

private:
    void buildSourceLocationMapLazy() const {
        if (!locMap) {
            locMap = SourceLocationMap::Build(src);
        }
    }

    std::filesystem::path p;
    std::string_view src;
    mutable std::optional<SourceLocationMap> locMap;
};

} // namespace prism

#endif // PRISM_SOURCE_SOURCECONTEXT_H
