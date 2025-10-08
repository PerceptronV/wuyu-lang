#include "numbers.h"
#include "../keywords.h"
#include <unordered_map>
#include <codecvt>
#include <locale>
#include <cstdlib>
#include <cctype>
#include <cmath>

namespace wuyu {
namespace utils {

namespace {
    // Helper function to trim ASCII whitespace
    inline std::string trimASCII(const std::string& s) {
        size_t i = 0, j = s.size();
        while (i < j && std::isspace(static_cast<unsigned char>(s[i]))) i++;
        while (j > i && std::isspace(static_cast<unsigned char>(s[j - 1]))) j--;
        return s.substr(i, j - i);
    }

    // Helper function to try parsing as ASCII integer
    inline bool tryParseASCIIInt(const std::string& s, long& out) {
        char* end = nullptr;
        const long v = std::strtol(s.c_str(), &end, 10);
        if (end && *end == '\0') { out = v; return true; }
        // Allow trailing spaces
        if (end) {
            while (*end && std::isspace(static_cast<unsigned char>(*end))) end++;
            if (*end == '\0') { out = v; return true; }
        }
        return false;
    }

    // Helper function to try parsing as ASCII double
    inline bool tryParseASCIIDouble(const std::string& s, double& out) {
        char* end = nullptr;
        const double v = std::strtod(s.c_str(), &end);
        if (end && *end == '\0') { out = v; return true; }
        if (end) {
            while (*end && std::isspace(static_cast<unsigned char>(*end))) end++;
            if (*end == '\0') { out = v; return true; }
        }
        return false;
    }

    // UTF-8 to UTF-32 converter
    inline std::u32string toU32(const char* utf8) {
        static thread_local std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
        return conv.from_bytes(utf8 ? std::string(utf8) : std::string());
    }

    inline std::u32string toU32(const std::string& utf8) {
        static thread_local std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
        return conv.from_bytes(utf8);
    }
}

int64_t parseChineseInteger(const std::u32string& s) {
    int64_t result = 0;
    int64_t current = 0;
    int64_t section = 0; // Handle units like 萬/億 sections
    bool negative = false;

    size_t startIdx = 0;
    // Check for negative sign: 負 (traditional) or 负 (simplified)
    if (!s.empty() && (s[0] == U'負' || s[0] == U'负')) {
        negative = true;
        startIdx = 1;
    }

    for (size_t i = startIdx; i < s.size(); ++i) {
        char32_t c = s[i];
        
        // Check if it's a digit
        auto dIt = CHINESE_DIGITS.find(c);
        if (dIt != CHINESE_DIGITS.end()) {
            int digit = dIt->second;
            if (digit == -1) {
                // Decimal point marker (點), stop integer parsing
                break;
            }
            current = current * 10 + digit;
            continue;
        }
        
        // Check if it's a multiplier
        auto mIt = CHINESE_MULTIPLIERS.find(c);
        if (mIt != CHINESE_MULTIPLIERS.end()) {
            int64_t unit = mIt->second;
            if (unit >= 10000) {
                // Large unit (萬, 億) closes a section
                section = (section + (current == 0 ? 1 : current)) * unit;
                result += section;
                section = 0;
                current = 0;
            } else {
                // Small unit (十, 百, 千)
                int64_t factor = (current == 0 ? 1 : current);
                section += factor * unit;
                current = 0;
            }
            continue;
        }
        
        // Ignore other characters (whitespace, etc.)
    }
    
    int64_t total = result + section + current;
    return negative ? -total : total;
}

double parseChineseFloat(const std::u32string& s) {
    // Find decimal point: 點 (traditional) or 点 (simplified) or ASCII '.'
    size_t posDot = s.find(U'點');
    if (posDot == std::u32string::npos) posDot = s.find(U'点');
    if (posDot == std::u32string::npos) posDot = s.find(U'.');

    // If no decimal point, parse as integer
    if (posDot == std::u32string::npos) {
        return static_cast<double>(parseChineseInteger(s));
    }

    // Split into integer and fractional parts
    std::u32string intPart = s.substr(0, posDot);
    std::u32string fracPart = s.substr(posDot + 1);

    double integerValue = static_cast<double>(parseChineseInteger(intPart));
    double fractionalValue = 0.0;
    double base = 0.1;
    
    // Parse fractional part digit by digit
    for (char32_t c : fracPart) {
        auto it = CHINESE_DIGITS.find(c);
        if (it == CHINESE_DIGITS.end() || it->second == -1) {
            // Try ASCII digit
            if (c >= U'0' && c <= U'9') {
                fractionalValue += (c - U'0') * base;
                base *= 0.1;
                continue;
            }
            break;
        }
        fractionalValue += it->second * base;
        base *= 0.1;
    }
    
    // Handle negative fractional values
    if (integerValue < 0) {
        return -(std::abs(integerValue) + fractionalValue);
    }
    return integerValue + fractionalValue;
}

long parseIntegerUTF8(const char* utf8) {
    // First try ASCII parse
    const std::string sTrim = trimASCII(utf8 ? std::string(utf8) : std::string());
    long asciiVal = 0;
    if (tryParseASCIIInt(sTrim, asciiVal)) return asciiVal;
    
    // Fallback to Chinese parsing
    std::u32string u32 = toU32(sTrim.c_str());
    return static_cast<long>(parseChineseInteger(u32));
}

double parseFloatUTF8(const char* utf8) {
    // First try ASCII float
    const std::string sTrim = trimASCII(utf8 ? std::string(utf8) : std::string());
    double asciiVal = 0.0;
    if (tryParseASCIIDouble(sTrim, asciiVal)) return asciiVal;
    
    // Fallback to Chinese parsing
    std::u32string u32 = toU32(sTrim.c_str());
    return parseChineseFloat(u32);
}

bool isChineseNumberChar(char32_t c) {
    return CHINESE_DIGITS.count(c) > 0 || CHINESE_MULTIPLIERS.count(c) > 0;
}

bool containsChineseNumber(const std::string& utf8) {
    std::u32string u32 = toU32(utf8);
    for (char32_t c : u32) {
        if (isChineseNumberChar(c)) {
            return true;
        }
    }
    return false;
}

} // namespace utils
} // namespace wuyu

