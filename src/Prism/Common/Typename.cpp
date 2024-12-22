#include "Prism/Common/Typename.h"

#ifdef __GNUC__
#include <cstdlib>

#include <cxxabi.h>
#endif

using namespace prism;

std::string detail::demangleName(char const* mangled) {
    int status = 0;
    size_t length = 0;
    char* unmangled = abi::__cxa_demangle(mangled, nullptr, &length, &status);
    if (status != 0) return mangled;
    std::string result(unmangled, length);
    std::free(unmangled);
    return result;
}
