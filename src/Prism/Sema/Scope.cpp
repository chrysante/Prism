#include "Prism/Sema/Scope.h"

#include <algorithm>

#include "Prism/Common/Assert.h"
#include "Prism/Sema/Symbol.h"

using namespace prism;

std::span<Symbol const* const> Scope::symbolsByName(
    std::string_view name) const {
    auto itr = _names.find(name);
    if (itr != _names.end()) return itr->second;
    return {};
}

namespace {

template <typename T, typename F>
struct CallbackIterator {
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using pointer = T*;
    using reference = T&;
    using iterator_category = std::output_iterator_tag;

    explicit CallbackIterator(F callback): callback(callback) {}

    CallbackIterator& operator*() { return *this; }

    CallbackIterator& operator++() { return *this; }
    CallbackIterator operator++(int) { return *this; }

    CallbackIterator& operator=(T const& value) {
        callback(value);
        return *this;
    }

    CallbackIterator& operator=(T&& value) {
        callback(std::move(value));
        return *this;
    }

    F callback;
};

} // namespace

template <typename T, typename F>
static CallbackIterator<T, F> makeCallbackIterator(F f) {
    return CallbackIterator<T, F>(f);
}

static size_t computeAcceptedDistance(size_t nameSize) {
    return std::clamp(nameSize / 3, size_t{ 1 }, size_t{ 4 });
}

template <typename S, typename Map>
static utl::small_vector<S*> symbolsByApproxNameImpl(std::string_view name,
                                                     Map const& map) {
    utl::small_vector<S*> result;
    using ItrType = Map::const_iterator;
    // Must use a manually defined callback type here, because for some reason
    // C++ lambda types have no assignment operators
    struct Callback {
        void operator()(ItrType itr) const {
            r->insert(r->end(), itr->value().begin(), itr->value().end());
        }
        utl::small_vector<S*>* r;
    };
    map.lookup(name, computeAcceptedDistance(name.size()),
               makeCallbackIterator<ItrType>(Callback{ &result }));
    return result;
}

utl::small_vector<Symbol*> Scope::symbolsByApproxName(std::string_view name) {
    return symbolsByApproxNameImpl<Symbol>(name, _approxNames);
}

utl::small_vector<Symbol const*> Scope::symbolsByApproxName(
    std::string_view name) const {
    return symbolsByApproxNameImpl<Symbol const>(name, _approxNames);
}

void Scope::addSymbol(Symbol& symbol) {
    auto [itr, success] = _symbols.insert(&symbol);
    PRISM_ASSERT(success, "Symbol has already been added to this scope");
    if (symbol.name().empty()) return;
    _names[symbol.name()].push_back(&symbol);
    _approxNames[symbol.name()].push_back(&symbol);
}
