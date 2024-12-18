#include "Prism/Common/IndentingStreambuf.h"

#include <ostream>

using namespace prism;

IndentingStreambufBase::IndentingStreambufBase(std::streambuf* wrappedBuffer,
                                               bool indentFirstLine):
    wrappedBuffer(wrappedBuffer), atLineStart(indentFirstLine) {}

int IndentingStreambufBase::overflow(int ch) {
    if (ch == EOF) {
        return EOF;
    }
    if (atLineStart) {
        atLineStart = false;
        writeIndentation(wrappedBuffer);
    }
    processCharacter((char)ch);
    return wrappedBuffer->sputc(ch);
}

void IndentingStreambufBase::processCharacter(char c) {
    if (inEscapeSequence) {
        if (c >= '@' && c <= '~') {
            // End of escape sequence
            inEscapeSequence = false;
        }
    }
    else if (utf8BytesRemaining > 0) {
        --utf8BytesRemaining;
    }
    else if (c == '\033') {
        inEscapeSequence = true;
    }
    else if ((c & 0x80) == 0) {
        // ASCII character
        if (c == '\n') {
            atLineStart = true;
        }
    }
    else if ((c & 0xE0) == 0xC0) {
        // Start of a 2-byte UTF-8 sequence
        utf8BytesRemaining = 1;
    }
    else if ((c & 0xF0) == 0xE0) {
        // Start of a 3-byte UTF-8 sequence
        utf8BytesRemaining = 2;
    }
    else if ((c & 0xF8) == 0xF0) {
        // Start of a 4-byte UTF-8 sequence
        utf8BytesRemaining = 3;
    }
}

void DefaultIndenter::operator()(std::streambuf* buf) const {
    int end = std::max(level * spacesPerLevel, 0);
    for (size_t i = 0; i < end; ++i)
        buf->sputc(' ');
}

OstreamBufferGuard::OstreamBufferGuard(std::ostream& ostream,
                                       std::streambuf* newBuffer):
    ostream(ostream), stashed(ostream.rdbuf()) {
    ostream.rdbuf(newBuffer);
}

OstreamBufferGuard::~OstreamBufferGuard() { ostream.rdbuf(stashed); }
