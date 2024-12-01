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

/// Style structure used by `TreeFormatter`
struct TreeStyle {
    static TreeStyle const Default;

    enum LineStyle { Ascii, Sharp, Rounded };

    LineStyle lines;
};

inline constexpr TreeStyle TreeStyle::Default = { .lines = TreeStyle::Rounded };

/// Helper class for pretty-printing trees
class TreeFormatter {
public:
    enum Level : uint8_t {
        ChildBegin,
        ChildContinue,
        LastChildBegin,
        LastChildContinue
    };

    explicit TreeFormatter(std::ostream& ostr,
                           TreeStyle style = TreeStyle::Default):
        style(style), ostr(ostr), buf(ostr, Indenter{ this }) {}

    /// Wraps a call to function that writes details of the current node to the
    /// associated ostream
    void writeDetails(bool isLeaf, std::invocable auto&& writeCallback) {
        levels.push_back(isLeaf ? LastChildContinue : ChildContinue);
        std::invoke(std::forward<decltype(writeCallback)>(writeCallback));
        levels.pop_back();
    }

    /// Used to print a single child node. Handles indentation and edge drawing
    void writeChild(std::invocable auto&& writeCallback) {
        levels.push_back(LastChildBegin);
        std::invoke(std::forward<decltype(writeCallback)>(writeCallback));
        levels.pop_back();
    }

    /// Used to print a range of child nodes. Handles indentation and edge
    /// drawing.
    ///
    /// \param children A range of children of the current node
    /// \param writeCallback Invocable with elements of the range of children
    /// used to print child nodes
    template <ranges::sized_range R>
    void writeChildren(
        R&& children,
        std::invocable<ranges::range_value_t<R>> auto&& writeCallback) {
        writeChildren(std::forward<R>(children),
                      [&]<typename T>(size_t, T&& elem) {
            std::invoke(std::forward<decltype(writeCallback)>(writeCallback),
                        std::forward<T>(elem));
        });
    }

    /// \overload for callbacks with indices
    template <ranges::sized_range R>
    void writeChildren(
        R&& children,
        std::invocable<size_t, ranges::range_value_t<R>> auto&& writeCallback) {
        size_t size = ranges::size(children);
        levels.push_back({});
        for (auto [index, elem]: children | ranges::views::enumerate) {
            levels.back() = index + 1 != size ? ChildBegin : LastChildBegin;
            std::invoke(std::forward<decltype(writeCallback)>(writeCallback),
                        index, std::forward<decltype(elem)>(elem));
        }
        levels.pop_back();
    }

    /// \Returns the underlying `std::ostream`
    std::ostream& ostream() const { return ostr; }

private:
    struct Indenter {
        void operator()(std::streambuf* buf) const;
        TreeFormatter* fmt;
    };

    TreeStyle style;
    std::ostream& ostr;
    IndentingStreambuf<Indenter> buf;
    utl::small_vector<Level> levels;
};

} // namespace prism

#endif // PRISM_COMMON_TREEFORMATTER_H
