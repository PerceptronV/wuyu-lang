#pragma once

#include "keywords.h"
#include <string>
#include <iostream>

namespace wuyu {

struct Token {
    TokenType type;
    std::string lexeme;
    int line;
    int column;
    
    // For literals
    union {
        int64_t intValue;
        double floatValue;
        bool boolValue;
    };
    std::string stringValue;
    
    Token(TokenType type = TokenType::UNKNOWN, const std::string& lexeme = "", 
          int line = 0, int column = 0)
        : type(type), lexeme(lexeme), line(line), column(column), intValue(0) {}
    
    void print() const {
        std::cout << "Token(" << tokenTypeToString(type) << ", \"" << lexeme 
                  << "\", " << line << ":" << column << ")";
        if (type == TokenType::INTEGER) {
            std::cout << " = " << intValue;
        } else if (type == TokenType::FLOAT) {
            std::cout << " = " << floatValue;
        } else if (type == TokenType::STRING) {
            std::cout << " = \"" << stringValue << "\"";
        }
        std::cout << std::endl;
    }
};

} // namespace wuyu

