#include "Prism/Common/IndentingStreambuf.h"

#include <ostream>

using namespace prism;

IndentingStreambufBase::IndentingStreambufBase(std::ostream& ostr,
                                               bool indentFirstLine):
    ostr(ostr), wrappedBuffer(ostr.rdbuf()), at_line_start(indentFirstLine) {
    ostr.rdbuf(this);
}

IndentingStreambufBase::~IndentingStreambufBase() { ostr.rdbuf(wrappedBuffer); }

int IndentingStreambufBase::overflow(int ch) {
    if (ch == EOF) {
        return EOF;
    }
    if (at_line_start) {
        at_line_start = false;
        writeIndentation(wrappedBuffer);
    }
    processCharacter((char)ch);
    return wrappedBuffer->sputc(ch);
}

void IndentingStreambufBase::processCharacter(char c) {
    if (in_escape_sequence) {
        if (c >= '@' && c <= '~') {
            // End of escape sequence
            in_escape_sequence = false;
        }
    }
    else if (utf8_bytes_remaining > 0) {
        --utf8_bytes_remaining;
    }
    else if (c == '\033') {
        in_escape_sequence = true;
    }
    else if ((c & 0x80) == 0) {
        // ASCII character
        if (c == '\n') {
            at_line_start = true;
        }
    }
    else if ((c & 0xE0) == 0xC0) {
        // Start of a 2-byte UTF-8 sequence
        utf8_bytes_remaining = 1;
    }
    else if ((c & 0xF0) == 0xE0) {
        // Start of a 3-byte UTF-8 sequence
        utf8_bytes_remaining = 2;
    }
    else if ((c & 0xF8) == 0xF0) {
        // Start of a 4-byte UTF-8 sequence
        utf8_bytes_remaining = 3;
    }
}
