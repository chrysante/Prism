#ifndef PRISM_COMMON_TREEFORMATTER_H
#define PRISM_COMMON_TREEFORMATTER_H

#include <concepts>
#include <ostream>
#include <streambuf>
#include <string_view>

#include <range/v3/view.hpp>
#include <utl/vector.hpp>

#include <Prism/Common/IndentingStreambuf.h>

namespace prism {

/// Helper class for pretty-printing trees
class TreeFormatter {
public:
    enum Level : uint8_t {
        ChildBegin,
        ChildContinue,
        LastChildBegin,
        LastChildContinue
    };

    explicit TreeFormatter(std::ostream& ostr):
        ostr(ostr), buf(ostr, Indenter{ this }) {}

    /// Wraps a call to function that writes details of the current node to the
    /// associated ostream
    template <std::invocable F>
    void writeDetails(bool isLeaf, F&& writeCallback) {
        levels.push_back(isLeaf ? LastChildContinue : ChildContinue);
        std::invoke(writeCallback);
        levels.pop_back();
    }

    /// Children must be printed using recursive calls to this function. Handles
    /// indentation and edge drawing.
    ///
    /// \param children A range of children of the current node
    /// \param writeCallback Invocable with elements of the range of children
    /// used to print child nodes
    template <ranges::sized_range R, std::invocable<ranges::range_value_t<R>> F>
    void writeChildren(R&& children, F&& writeCallback) {
        writeChildren(std::forward<R>(children),
                      [&]<typename T>(size_t, T&& elem) {
            std::invoke(writeCallback, std::forward<T>(elem));
        });
    }

    /// \overload
    template <ranges::sized_range R,
              std::invocable<size_t, ranges::range_value_t<R>> F>
    void writeChildren(R&& children, F&& writeCallback) {
        size_t size = ranges::size(children);
        levels.push_back({});
        for (auto [index, elem]: children | ranges::views::enumerate) {
            levels.back() = index + 1 != size ? ChildBegin : LastChildBegin;
            std::invoke(writeCallback, index,
                        std::forward<decltype(elem)>(elem));
        }
        levels.pop_back();
    }

private:
    struct Indenter {
        void operator()(std::streambuf* buf) const;
        TreeFormatter* fmt;
    };

    std::ostream& ostr;
    IndentingStreambuf<Indenter> buf;
    utl::small_vector<Level> levels;
};

} // namespace prism

#endif // PRISM_COMMON_TREEFORMATTER_H
