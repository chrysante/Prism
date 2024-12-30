#include "Prism/Source/SourceContext.h"

#include "Prism/Common/Assert.h"

using namespace prism;

SourceLocationMap SourceLocationMap::Build(std::string_view source) {
    SourceLocationMap result;
    uint32_t i = 0, line = 0;
    for (; i < source.size(); ++i) {
        if (source[i] == '\n') {
            result.map.push_back({ i, line++ });
        }
    }
    result.map.push_back({ i, line });
    return result;
}

SourceLocation SourceLocationMap::operator[](uint32_t index) const {
    auto itr =
        std::lower_bound(map.begin(), map.end(), std::pair{ index, 0u },
                         [](auto a, auto b) { return a.first < b.first; });
    PRISM_ASSERT(
        itr != map.end(),
        "index not found. is index out of bounds for this source file?");
    uint32_t lineBeginIdx = itr == map.begin() ? 0 : std::prev(itr)->first + 1;
    return { .index = index,
             .line = itr->second,
             .column = index - lineBeginIdx };
}

SourceLocation SourceContext::getSourceLocation(uint32_t index) const {
    buildSourceLocationMapLazy();
    return (*locMap)[index];
}

FullSourceRange SourceContext::getFullSourceRange(SourceRange range) const {
    auto [index, len] = range;
    auto begin = getSourceLocation(index);
    auto end = getSourceLocation(index + len);
    return FullSourceRange{ this, begin, end };
}
