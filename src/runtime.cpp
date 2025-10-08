// Runtime helpers for parsing Chinese and ASCII numerals from UTF-8 strings
// Now using unified implementation from utils/numbers.h
#include "utils/numbers.h"

extern "C" long wuyu_parse_int_utf8(const char* utf8) {
    return wuyu::utils::parseIntegerUTF8(utf8);
}

extern "C" double wuyu_parse_float_utf8(const char* utf8) {
    return wuyu::utils::parseFloatUTF8(utf8);
}


