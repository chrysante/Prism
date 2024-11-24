#ifndef PRISM_SOURCE_SOURCECONTEXT_H
#define PRISM_SOURCE_SOURCECONTEXT_H

#include <cstdint>
#include <string_view>
#include <utility>
#include <vector>

#include <Prism/Source/SourceLocation.h>

namespace prism {

class SourceLocationMap {
public:
    static SourceLocationMap Build(std::string_view source);

    SourceLocationMap() = default;

    SourceLocation operator[](uint32_t index) const;

private:
    std::vector<std::pair<uint32_t, uint32_t>> map;
};

class SourceContext {
public:
    explicit SourceContext(std::string_view source): source(source) {}

    void buildSourceLocationMap() { locMap = SourceLocationMap::Build(source); }

    SourceLocation getSourceLocation(uint32_t index) const {
        return locMap[index];
    }

private:
    std::string_view source;
    SourceLocationMap locMap;
};

} // namespace prism

#endif // PRISM_SOURCE_SOURCECONTEXT_H
