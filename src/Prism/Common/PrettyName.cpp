#include "Prism/Common/PrettyName.h"

#include <cctype>

using namespace prism;

std::string prism::prettyName(std::string_view name,
                              PrettyNameOptions options) {
    std::string result;
    result.reserve(name.size());
    bool first = true;
    bool newWord = false; // Indicates the start of a new word
    for (size_t i = 0; i < name.size(); ++i) {
        char c = name[i];
        // If the character is an underscore or transition from lowercase to
        // uppercase
        if (c == '_' || (i > 0 && std::islower(name[i - 1]) && std::isupper(c)))
        {
            result += ' ';
            newWord = true;
        }
        else if (std::isupper(c) && i > 0 && std::isupper(name[i - 1]) &&
                 (i + 1 < name.size() && std::islower(name[i + 1])))
        {
            // Split words if an uppercase letter is followed by a
            // lowercase letter
            result += ' ';
            newWord = true;
        }
        else if (c == '_') {
            continue; // Skip underscores
        }
        if (first) {
            result += options.capitalizeFirst ? (char)std::toupper(c) :
                                                (char)std::tolower(c);
            first = false;
        }
        else if (newWord) {
            result += options.capitalizeRest ? (char)std::toupper(c) :
                                               (char)std::tolower(c);
            newWord = false;
        }
        else {
            result += (char)std::tolower(c);
        }
    }
    return result;
}
