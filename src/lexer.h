#pragma once

#include "token.h"
#include "keywords.h"
#include "utils/numbers.h"
#include <vector>
#include <string>
#include <stack>
#include <codecvt>
#include <locale>

namespace wuyu {

class Lexer {
private:
    std::string source;
    std::u32string u32source;  // UTF-32 for easier character handling
    size_t position;
    size_t line;
    size_t column;
    std::stack<int> indentStack;
    std::vector<Token> tokens;
    bool atLineStart;
    
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> converter;
    
public:
    Lexer(const std::string& source) 
        : source(source), position(0), line(1), column(1), atLineStart(true) {
        indentStack.push(0);
        u32source = converter.from_bytes(source);
    }
    
    std::vector<Token> tokenize() {
        tokens.clear();
        position = 0;
        line = 1;
        column = 1;
        atLineStart = true;
        
        while (!isAtEnd()) {
            if (atLineStart) {
                handleIndentation();
            }
            scanToken();
        }
        
        // Emit remaining dedents
        while (indentStack.size() > 1) {
            indentStack.pop();
            tokens.push_back(Token(TokenType::DEDENT, "", line, column));
        }
        
        tokens.push_back(Token(TokenType::END_OF_FILE, "", line, column));
        return tokens;
    }
    
private:
    bool isAtEnd() const {
        return position >= u32source.length();
    }
    
    char32_t advance() {
        column++;
        return u32source[position++];
    }
    
    char32_t peek() const {
        if (isAtEnd()) return U'\0';
        return u32source[position];
    }
    
    char32_t peekNext() const {
        if (position + 1 >= u32source.length()) return U'\0';
        return u32source[position + 1];
    }
    
    std::string utf32ToUtf8(const std::u32string& str) {
        return converter.to_bytes(str);
    }
    
    std::string utf32ToUtf8(char32_t ch) {
        return converter.to_bytes(&ch, &ch + 1);
    }
    
    void handleIndentation() {
        int indent = 0;
        
        // Count spaces/tabs
        while (!isAtEnd() && (peek() == U' ' || peek() == U'\t')) {
            if (peek() == U'\t') {
                indent += 4;
            } else {
                indent++;
            }
            advance();
        }
        
        // Skip blank lines and comments
        if (isAtEnd() || peek() == U'\n' || peek() == Char32_t_keywords::COMMENT) {
            return;
        }
        
        atLineStart = false;
        
        // Handle indent/dedent
        if (indent > indentStack.top()) {
            indentStack.push(indent);
            tokens.push_back(Token(TokenType::INDENT, "", line, column));
        } else if (indent < indentStack.top()) {
            while (indentStack.size() > 1 && indent < indentStack.top()) {
                indentStack.pop();
                tokens.push_back(Token(TokenType::DEDENT, "", line, column));
            }
            if (indent != indentStack.top()) {
                // Indentation error - for now, just continue
            }
        }
    }
    
    void scanToken() {
        char32_t c = peek();
        
        // Skip whitespace (except newlines)
        if (c == U' ' || c == Char32_t_keywords::SEPARATOR || c == U'\t') {
            advance();
            return;
        }
        
        // Handle newlines
        if (c == U'\n') {
            advance();
            // Only emit newline if we have real tokens
            if (!tokens.empty() && tokens.back().type != TokenType::NEWLINE) {
                tokens.push_back(Token(TokenType::NEWLINE, "\\n", line, column));
            }
            line++;
            column = 1;
            atLineStart = true;
            return;
        }
        
        // Comments
        if (c == Char32_t_keywords::COMMENT) {
            while (!isAtEnd() && peek() != U'\n') {
                advance();
            }
            return;
        }
        
        // String literals
        if (c == Char32_t_keywords::LSTRING1 || c == Char32_t_keywords::LSTRING2) {
            scanString(c);
            return;
        }
        
        // Check for multi-character Chinese keywords
        std::string potential = scanChineseWord();
        if (!potential.empty()) {
            // Check if it's a keyword
            auto it = KEYWORDS.find(potential);
            if (it != KEYWORDS.end()) {
                Token token(it->second, potential, line, column);
                if (it->second == TokenType::BOOL_TRUE) {
                    token.boolValue = true;
                } else if (it->second == TokenType::BOOL_FALSE) {
                    token.boolValue = false;
                }
                tokens.push_back(token);
                return;
            }
            
            // Check for punctuation
            auto pit = PUNCTUATION.find(potential);
            if (pit != PUNCTUATION.end()) {
                tokens.push_back(Token(pit->second, potential, line, column));
                return;
            }
            
            // Check if it's a Chinese number
            if (isChineseNumber(potential)) {
                scanChineseNumber(potential);
                return;
            }
            
            // Otherwise, it's an identifier
            tokens.push_back(Token(TokenType::IDENTIFIER, potential, line, column));
            return;
        }
        
        // ASCII numbers
        if (isDigit(c)) {
            scanNumber();
            return;
        }
        
        // ASCII identifiers (for mixed code)
        if (isAlpha(c) || c == U'_') {
            scanIdentifier();
            return;
        }
        
        // Unknown character - skip it
        advance();
    }
    
    std::string scanChineseWord() {
        std::u32string word;
        
        while (!isAtEnd() && isChinese(peek())) {
            // Check if current character alone is punctuation or keyword - if so, stop here if we have a word
            if (!word.empty()) {
                std::string singleChar = utf32ToUtf8(peek());
                if (PUNCTUATION.count(singleChar) || KEYWORDS.count(singleChar)) {
                    // Current char is punctuation/keyword and we have a word, return the word
                    break;
                }
            }
            
            word += advance();
            
            // Check if we've formed a keyword or punctuation
            std::string utf8Word = utf32ToUtf8(word);
            if (KEYWORDS.count(utf8Word) || PUNCTUATION.count(utf8Word)) {
                // Check if the next character would extend this into a longer keyword
                if (!isAtEnd() && isChinese(peek())) {
                    std::u32string nextWord = word + peek();
                    std::string nextUtf8 = utf32ToUtf8(nextWord);
                    if (KEYWORDS.count(nextUtf8) || PUNCTUATION.count(nextUtf8)) {
                        continue; // Keep going
                    }
                }
                return utf8Word;
            }
        }
        
        return utf32ToUtf8(word);
    }
    
    void scanString(char32_t quote) {
        advance(); // Skip opening quote
        std::u32string value;
        char32_t closeQuote = (quote == Char32_t_keywords::LSTRING1) ? Char32_t_keywords::RSTRING1 : Char32_t_keywords::RSTRING2;
        
        while (!isAtEnd() && peek() != closeQuote) {
            if (peek() == U'\n') {
                line++;
                column = 1;
            }
            value += advance();
        }
        
        if (isAtEnd()) {
            // Unterminated string
            tokens.push_back(Token(TokenType::STRING, utf32ToUtf8(value), line, column));
            return;
        }
        
        advance(); // Closing quote
        Token token(TokenType::STRING, utf32ToUtf8(value), line, column);
        token.stringValue = utf32ToUtf8(value);
        tokens.push_back(token);
    }
    
    void scanNumber() {
        size_t start = position;
        while (!isAtEnd() && isDigit(peek())) {
            advance();
        }
        
        bool isFloat = false;
        if (!isAtEnd() && peek() == U'.' && isDigit(peekNext())) {
            isFloat = true;
            advance(); // consume '.'
            while (!isAtEnd() && isDigit(peek())) {
                advance();
            }
        }
        
        std::string numStr = utf32ToUtf8(u32source.substr(start, position - start));
        Token token(isFloat ? TokenType::FLOAT : TokenType::INTEGER, numStr, line, column);
        
        if (isFloat) {
            token.floatValue = std::stod(numStr);
        } else {
            token.intValue = std::stoll(numStr);
        }
        
        tokens.push_back(token);
    }
    
    void scanIdentifier() {
        size_t start = position;
        while (!isAtEnd() && (isAlphaNumeric(peek()) || peek() == U'_')) {
            advance();
        }
        
        std::string text = utf32ToUtf8(u32source.substr(start, position - start));
        tokens.push_back(Token(TokenType::IDENTIFIER, text, line, column));
    }
    
    bool isChineseNumber(const std::string& str) {
        return utils::containsChineseNumber(str);
    }
    
    void scanChineseNumber(const std::string& str) {
        // Use unified Chinese number parsing from utils/numbers.h
        std::u32string u32str = converter.from_bytes(str);
        
        // Check if it contains a decimal point (點)
        bool hasDecimal = (u32str.find(CHINESE_DECIMAL_POINT) != std::u32string::npos) || 
                          (u32str.find(U'.') != std::u32string::npos);
        
        Token token(hasDecimal ? TokenType::FLOAT : TokenType::INTEGER, str, line, column);
        if (hasDecimal) {
            token.floatValue = utils::parseChineseFloat(u32str);
        } else {
            token.intValue = utils::parseChineseInteger(u32str);
        }
        tokens.push_back(token);
    }
    
    bool isChinese(char32_t c) const {
        // Basic CJK Unified Ideographs range
        return (c >= 0x4E00 && c <= 0x9FFF) || 
               (c >= 0x3400 && c <= 0x4DBF) ||
               (c >= 0x20000 && c <= 0x2A6DF) ||
               // Fullwidth and Halfwidth Forms (includes ：、，。《》「」『』etc.)
               (c >= 0xFF00 && c <= 0xFFEF) ||
               // CJK Symbols and Punctuation
               (c >= 0x3000 && c <= 0x303F);
    }
    
    bool isDigit(char32_t c) const {
        return c >= U'0' && c <= U'9';
    }
    
    bool isAlpha(char32_t c) const {
        return (c >= U'a' && c <= U'z') || (c >= U'A' && c <= U'Z');
    }
    
    bool isAlphaNumeric(char32_t c) const {
        return isAlpha(c) || isDigit(c);
    }
};

} // namespace wuyu

