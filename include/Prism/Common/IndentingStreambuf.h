#ifndef PRISM_COMMON_INDENTINGSTREAMBUF_H
#define PRISM_COMMON_INDENTINGSTREAMBUF_H

#include <concepts>
#include <iosfwd>
#include <streambuf>

namespace prism {

class IndentingStreambufBase: public std::streambuf {
protected:
    IndentingStreambufBase(std::ostream& ostr, bool indentFirstLine);

    ~IndentingStreambufBase();

private:
    virtual void writeIndentation(std::streambuf* buf) = 0;

    int overflow(int ch) override;

    void processCharacter(char c);

    std::ostream& ostr;
    std::streambuf* wrappedBuffer;
    bool at_line_start;
    bool in_escape_sequence = false;
    unsigned utf8_bytes_remaining = 0;
};

template <std::invocable<std::streambuf*> Indenter>
class IndentingStreambuf: public IndentingStreambufBase {
public:
    IndentingStreambuf(std::ostream& ostr, Indenter indenter,
                       bool indentFirstLine = true):
        IndentingStreambufBase(ostr, indentFirstLine), indenter(indenter) {}

private:
    void writeIndentation(std::streambuf* buf) override {
        std::invoke(indenter, buf);
    }

    Indenter indenter;
};

} // namespace prism

#endif // PRISM_COMMON_INDENTINGSTREAMBUF_H
