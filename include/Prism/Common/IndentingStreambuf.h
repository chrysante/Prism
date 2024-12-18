#ifndef PRISM_COMMON_INDENTINGSTREAMBUF_H
#define PRISM_COMMON_INDENTINGSTREAMBUF_H

#include <concepts>
#include <streambuf>

namespace prism {

/// Wraps a `std::streambuf*` and calls `writeIndentation()` before writing the
/// next character after a line break
class IndentingStreambufBase: public std::streambuf {
protected:
    explicit IndentingStreambufBase(std::streambuf* wrappedBuffer,
                                    bool indentFirstLine);

private:
    /// Customization point
    virtual void writeIndentation(std::streambuf* buf) = 0;

    int overflow(int ch) override;

    void processCharacter(char c);

    std::streambuf* wrappedBuffer;
    bool atLineStart;
    bool inEscapeSequence = false;
    unsigned utf8BytesRemaining = 0;
};

/// Convenience indenting streambuf that invokes `Indenter::operator()` on
/// indentation
template <std::invocable<std::streambuf*> Indenter = struct DefaultIndenter>
class IndentingStreambuf: public IndentingStreambufBase, public Indenter {
public:
    IndentingStreambuf(std::streambuf* wrappedBuffer, Indenter indenter,
                       bool indentFirstLine = true):
        IndentingStreambufBase(wrappedBuffer, indentFirstLine),
        Indenter(indenter) {}

private:
    void writeIndentation(std::streambuf* buf) override {
        std::invoke(static_cast<Indenter&>(*this), buf);
    }
};

/// Default indenter for indenting streambufs
struct DefaultIndenter {
    int spacesPerLevel = 4;
    int level = 0;

    void increase() { ++level; }
    void decrease() { --level; }

    template <std::invocable F>
    void indended(F&& f) {
        increase();
        std::invoke(std::forward<F>(f));
        decrease();
    }

    void operator()(std::streambuf* buf) const;
};

/// On construction, replaces the stream buffer of `ostream` with `newBuffer`
/// On destruction, restores the old stream buffer
class OstreamBufferGuard {
public:
    explicit OstreamBufferGuard(std::ostream& ostream,
                                std::streambuf* newBuffer);
    ~OstreamBufferGuard();

private:
    std::ostream& ostream;
    std::streambuf* stashed;
};

} // namespace prism

#endif // PRISM_COMMON_INDENTINGSTREAMBUF_H
