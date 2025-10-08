#pragma once

#include <string>
#include <unordered_map>
#include <unordered_set>

namespace wuyu {

// Chinese keyword constants
namespace Keywords {
    // Type keywords
    const std::string INT = "數";
    const std::string FLOAT = "分";
    const std::string COMPLEX = "複";
    const std::string BOOL = "問";
    const std::string STRING = "文";
    const std::string ARRAY = "列";
    const std::string TENSOR = "陣";
    const std::string CLASS = "類";
    const std::string TYPE = "型";
    const std::string FUNCTION = "術";
    const std::string OBJECT = "物";
    
    // Boolean literals
    const std::string TRUE = "是";
    const std::string FALSE = "非";
    
    // Operator keywords
    const std::string ADD = "益";
    const std::string SUB = "損";
    const std::string NEG = "負";
    const std::string MUL = "乘";
    const std::string POW = "冪";
    const std::string DIV = "除";
    const std::string FLOORDIV = "拆";
    const std::string MOD = "餘";
    const std::string INC = "增";
    const std::string DEC = "削";
    const std::string EQ = "如";
    const std::string NEQ = "弗";
    const std::string GT = "盈";
    const std::string LT = "虧";
    const std::string GTE = "滿";
    const std::string LTE = "頂";
    const std::string ASSIGN = "為";
    
    // Control flow keywords
    const std::string FOR = "順";
    const std::string WHILE = "循";
    const std::string IF = "若";
    const std::string THEN = "則";
    const std::string ELSE_IF = "而";
    const std::string ELSE = "然";
    const std::string USING = "以";
    const std::string IN = "通";
    const std::string GRADUAL = "漸";
    const std::string TO = "至";
    const std::string LOOPER = "也";
    const std::string SOLIDIFIER = "者";
    
    // Declaration keywords
    const std::string LET = "有";
    const std::string DEF = "設";
    const std::string SELF = "己";
    const std::string ACCESS = "之";
    const std::string ALIAS = "號";
    const std::string RETURN = "奏";
    const std::string YIELD = "得";
    const std::string INHERIT = "承";
    const std::string IMPORT = "取";
    
    // Array/Function keywords
    const std::string ARRAY_DECL = "廣";
    const std::string INDEX = "其";
    const std::string PARAM = "參";
    const std::string CALL = "執";
    
    // Meta operator keywords
    const std::string UNARY = "單";
    const std::string BINARY = "雙";
    const std::string CARGO = "載";
    
    // IO keywords
    const std::string SAY = "曰";
    const std::string ASK = "求";
    const std::string NEWLINE_LIT = "更";
    
    // Error messages
    const std::string ERROR_PREFIX = "錯：";
    const std::string ERROR_NEEDS_PARAM = "需參數也。";
    const std::string ERROR_UNKNOWN_OPTION = "未知則令也。";

    // Punctuation
    const std::string COMMA = "、";
    const std::string COLON = "：";
    const std::string PERIOD = "。";
    const std::string LGROUP = "《";
    const std::string RGROUP = "》";
    const std::string LSTRING1 = "「";
    const std::string RSTRING1 = "」";
    const std::string LSTRING2 = "『";
    const std::string RSTRING2 = "』";
    const std::string LPAREN = "（";
    const std::string RPAREN = "）";
    const std::string SEPARATOR = "，";
    const std::string COMMENT = "·";
}

// Token types
enum class TokenType {
    // Literals
    INTEGER,
    FLOAT,
    STRING,
    BOOLEAN,
    
    // Identifiers
    IDENTIFIER,
    
    // Keywords
    KW_LET,         // 有
    KW_DEF,         // 設
    KW_SELF,        // 己
    KW_ACCESS,      // 之
    KW_ALIAS,       // 號
    KW_RETURN,      // 奏
    KW_YIELD,       // 得
    KW_INHERIT,     // 承
    KW_IMPORT,      // 取
    
    // Types
    TYPE_INT,       // 數
    TYPE_FLOAT,     // 分
    TYPE_COMPLEX,   // 複
    TYPE_STRING,    // 文
    TYPE_BOOL,      // 問
    TYPE_ARRAY,     // 列
    TYPE_TENSOR,    // 陣
    TYPE_CLASS,     // 類
    TYPE_TYPE,      // 型
    TYPE_FUNCTION,  // 術
    TYPE_OBJECT,    // 物
    
    // Boolean literals
    BOOL_TRUE,      // 是
    BOOL_FALSE,     // 非
    
    // Operators
    OP_ADD,         // 益
    OP_SUB,         // 損
    OP_NEG,         // 負 (unary minus)
    OP_MUL,         // 乘
    OP_POW,         // 冪
    OP_DIV,         // 除
    OP_FLOORDIV,    // 拆
    OP_MOD,         // 餘
    OP_INC,         // 增
    OP_DEC,         // 削
    
    // Assignment and comparison
    OP_ASSIGN,      // 為
    OP_EQ,          // 如
    OP_NEQ,         // 弗
    OP_GT,          // 盈
    OP_LT,          // 虧
    OP_GTE,         // 滿
    OP_LTE,         // 頂
    
    // Control flow
    KW_IF,          // 若
    KW_THEN,        // 則
    KW_ELSE_IF,     // 而
    KW_ELSE,        // 然
    KW_WHILE,       // 循
    KW_FOR,         // 順
    KW_IN,          // 通
    
    // Array/Function syntax
    KW_ARRAY_DECL,  // 廣...者
    KW_INDEX,       // 其...者
    KW_PARAM,       // 參...者
    KW_CALL,        // 執...者
    KW_GRADUAL,     // 漸
    KW_TO,          // 至
    KW_USING,       // 以
    KW_LOOPER,      // 也
    KW_SOLIDIFIER,  // 者
    
    // Meta operators
    KW_UNARY,       // 單
    KW_BINARY,      // 雙
    KW_CARGO,       // 載
    
    // Punctuation
    COMMA,          // 、
    COLON,          // ：
    PERIOD,         // 。
    LGROUP,         // 《
    RGROUP,         // 》
    LSTRING1,       // 「
    RSTRING1,       // 」
    LSTRING2,       // 『
    RSTRING2,       // 』
    LPAREN,         // (
    RPAREN,         // )
        
    // Special
    NEWLINE,
    INDENT,
    DEDENT,
    END_OF_FILE,
    SEPARATOR,      // ，
    COMMENT,        // ·
    
    // IO
    KW_SAY,         // 曰
    KW_ASK,         // 求
    LIT_NEWLINE,    // 更 (newline literal)
    
    UNKNOWN
};

// Chinese number characters
const std::unordered_map<char32_t, int> CHINESE_DIGITS = {
    {U'零', 0}, {U'一', 1}, {U'二', 2}, {U'三', 3}, {U'四', 4},
    {U'五', 5}, {U'六', 6}, {U'七', 7}, {U'八', 8}, {U'九', 9},
    {U'點', -1} // decimal point
};

const std::unordered_map<char32_t, int> CHINESE_MULTIPLIERS = {
    {U'十', 10},
    {U'百', 100},
    {U'千', 1000},
    {U'萬', 10000},
    {U'億', 100000000}
};

// Keywords mapping
const std::unordered_map<std::string, TokenType> KEYWORDS = {
    // Declarations and definitions
    {Keywords::LET, TokenType::KW_LET},
    {Keywords::DEF, TokenType::KW_DEF},
    {Keywords::SELF, TokenType::KW_SELF},
    {Keywords::ACCESS, TokenType::KW_ACCESS},
    {Keywords::ALIAS, TokenType::KW_ALIAS},
    {Keywords::RETURN, TokenType::KW_RETURN},
    {Keywords::YIELD, TokenType::KW_YIELD},
    {Keywords::INHERIT, TokenType::KW_INHERIT},
    {Keywords::IMPORT, TokenType::KW_IMPORT},
    
    // Types
    {Keywords::INT, TokenType::TYPE_INT},
    {Keywords::FLOAT, TokenType::TYPE_FLOAT},
    {Keywords::COMPLEX, TokenType::TYPE_COMPLEX},
    {Keywords::STRING, TokenType::TYPE_STRING},
    {Keywords::BOOL, TokenType::TYPE_BOOL},
    {Keywords::ARRAY, TokenType::TYPE_ARRAY},
    {Keywords::TENSOR, TokenType::TYPE_TENSOR},
    {Keywords::CLASS, TokenType::TYPE_CLASS},
    {Keywords::TYPE, TokenType::TYPE_TYPE},
    {Keywords::FUNCTION, TokenType::TYPE_FUNCTION},
    {Keywords::OBJECT, TokenType::TYPE_OBJECT},
    
    // Boolean
    {Keywords::TRUE, TokenType::BOOL_TRUE},
    {Keywords::FALSE, TokenType::BOOL_FALSE},
    
    // Operators
    {Keywords::ADD, TokenType::OP_ADD},
    {Keywords::SUB, TokenType::OP_SUB},
    {Keywords::NEG, TokenType::OP_NEG},
    {Keywords::MUL, TokenType::OP_MUL},
    {Keywords::POW, TokenType::OP_POW},
    {Keywords::DIV, TokenType::OP_DIV},
    {Keywords::FLOORDIV, TokenType::OP_FLOORDIV},
    {Keywords::MOD, TokenType::OP_MOD},
    {Keywords::INC, TokenType::OP_INC},
    {Keywords::DEC, TokenType::OP_DEC},
    
    // Assignment and comparison
    {Keywords::ASSIGN, TokenType::OP_ASSIGN},
    {Keywords::EQ, TokenType::OP_EQ},
    {Keywords::NEQ, TokenType::OP_NEQ},
    {Keywords::GT, TokenType::OP_GT},
    {Keywords::LT, TokenType::OP_LT},
    {Keywords::GTE, TokenType::OP_GTE},
    {Keywords::LTE, TokenType::OP_LTE},
    
    // Control flow
    {Keywords::IF, TokenType::KW_IF},
    {Keywords::THEN, TokenType::KW_THEN},
    {Keywords::ELSE_IF, TokenType::KW_ELSE_IF},
    {Keywords::ELSE, TokenType::KW_ELSE},
    {Keywords::WHILE, TokenType::KW_WHILE},
    {Keywords::FOR, TokenType::KW_FOR},
    {Keywords::USING, TokenType::KW_USING},
    {Keywords::IN, TokenType::KW_IN},
    {Keywords::GRADUAL, TokenType::KW_GRADUAL},
    {Keywords::TO, TokenType::KW_TO},
    {Keywords::LOOPER, TokenType::KW_LOOPER},
    {Keywords::SOLIDIFIER, TokenType::KW_SOLIDIFIER},
    
    // Special keywords
    {Keywords::ARRAY_DECL, TokenType::KW_ARRAY_DECL},
    {Keywords::INDEX, TokenType::KW_INDEX},
    {Keywords::PARAM, TokenType::KW_PARAM},
    {Keywords::CALL, TokenType::KW_CALL},
    
    // Meta operators
    {Keywords::UNARY, TokenType::KW_UNARY},
    {Keywords::BINARY, TokenType::KW_BINARY},
    {Keywords::CARGO, TokenType::KW_CARGO},
    
    // IO
    {Keywords::SAY, TokenType::KW_SAY},
    {Keywords::ASK, TokenType::KW_ASK},
    // Literal alias
    {Keywords::NEWLINE_LIT, TokenType::LIT_NEWLINE}
};

// Single-character tokens
const std::unordered_map<std::string, TokenType> PUNCTUATION = {
    {Keywords::COMMA, TokenType::COMMA},
    {Keywords::SEPARATOR, TokenType::SEPARATOR},
    {Keywords::COLON, TokenType::COLON},
    {Keywords::PERIOD, TokenType::PERIOD},
    {Keywords::LGROUP, TokenType::LGROUP},
    {Keywords::RGROUP, TokenType::RGROUP},
    {Keywords::LSTRING1, TokenType::LSTRING1},
    {Keywords::RSTRING1, TokenType::RSTRING1},
    {Keywords::LSTRING2, TokenType::LSTRING2},
    {Keywords::RSTRING2, TokenType::RSTRING2},
    {Keywords::COMMENT, TokenType::COMMENT},
    {Keywords::LPAREN, TokenType::LPAREN},
    {Keywords::RPAREN, TokenType::RPAREN}
};

inline std::string tokenTypeToString(TokenType type) {
    switch (type) {
        case TokenType::INTEGER: return "INTEGER";
        case TokenType::FLOAT: return "FLOAT";
        case TokenType::STRING: return "STRING";
        case TokenType::BOOLEAN: return "BOOLEAN";
        case TokenType::IDENTIFIER: return "IDENTIFIER";
        case TokenType::KW_LET: return "KW_LET";
        case TokenType::KW_DEF: return "KW_DEF";
        case TokenType::KW_IF: return "KW_IF";
        case TokenType::KW_THEN: return "KW_THEN";
        case TokenType::KW_SAY: return "KW_SAY";
        case TokenType::KW_ASK: return "KW_ASK";
        case TokenType::TYPE_FUNCTION: return "TYPE_FUNCTION";
        case TokenType::KW_PARAM: return "KW_PARAM";
        case TokenType::TYPE_INT: return "TYPE_INT";
        case TokenType::KW_LOOPER: return "KW_LOOPER";
        case TokenType::KW_YIELD: return "KW_YIELD";
        case TokenType::KW_RETURN: return "KW_RETURN";
        case TokenType::OP_ASSIGN: return "OP_ASSIGN";
        case TokenType::PERIOD: return "PERIOD";
        case TokenType::NEWLINE: return "NEWLINE";
        case TokenType::INDENT: return "INDENT";
        case TokenType::DEDENT: return "DEDENT";
        case TokenType::END_OF_FILE: return "EOF";
        default: return "UNKNOWN";
    }
}

} // namespace wuyu

