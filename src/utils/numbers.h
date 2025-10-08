#pragma once

#include <string>
#include <cstdint>

namespace wuyu {
namespace utils {

/**
 * Parse a Chinese or ASCII integer from a UTF-32 string.
 * Supports:
 * - Chinese digits: 零一二三四五六七八九
 * - Multipliers: 十百千萬億
 * - Negative sign: 負
 * - ASCII digits: 0-9 with optional minus sign
 * 
 * @param u32str UTF-32 encoded string
 * @return Parsed integer value
 */
int64_t parseChineseInteger(const std::u32string& u32str);

/**
 * Parse a Chinese or ASCII floating-point number from a UTF-32 string.
 * Supports:
 * - All integer features
 * - Decimal point: 點 or ASCII '.'
 * - Fractional digits in Chinese or ASCII
 * 
 * @param u32str UTF-32 encoded string
 * @return Parsed floating-point value
 */
double parseChineseFloat(const std::u32string& u32str);

/**
 * Parse an integer from UTF-8 encoded string.
 * First attempts ASCII parsing, then falls back to Chinese parsing.
 * 
 * @param utf8 UTF-8 encoded C-string
 * @return Parsed integer value (as long for C compatibility)
 */
long parseIntegerUTF8(const char* utf8);

/**
 * Parse a floating-point number from UTF-8 encoded string.
 * First attempts ASCII parsing, then falls back to Chinese parsing.
 * 
 * @param utf8 UTF-8 encoded C-string
 * @return Parsed floating-point value
 */
double parseFloatUTF8(const char* utf8);

/**
 * Check if a UTF-32 character is a Chinese digit or multiplier.
 * 
 * @param c UTF-32 character
 * @return true if the character is part of Chinese number system
 */
bool isChineseNumberChar(char32_t c);

/**
 * Check if a UTF-8 string contains Chinese number characters.
 * 
 * @param utf8 UTF-8 encoded string
 * @return true if the string contains Chinese number characters
 */
bool containsChineseNumber(const std::string& utf8);

} // namespace utils
} // namespace wuyu

