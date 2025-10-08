#pragma once

#include "ast.h"
#include "token.h"
#include "lexer.h"
#include "keywords.h"
#include <stdexcept>
#include <sstream>

namespace wuyu {

class Parser {
private:
    std::vector<Token> tokens;
    size_t current;
    
public:
    Parser(const std::vector<Token>& tokens) : tokens(tokens), current(0) {}
    
    std::unique_ptr<ProgramAST> parse() {
        std::vector<StmtPtr> statements;
        
        while (!isAtEnd()) {
            // Skip newlines at top level
            if (match(TokenType::NEWLINE)) continue;
            
            auto stmt = parseStatement();
            if (stmt) {
                statements.push_back(std::move(stmt));
            }
        }
        
        return std::make_unique<ProgramAST>(std::move(statements));
    }
    
private:
    // Token manipulation
    bool isAtEnd() const {
        return peek().type == TokenType::END_OF_FILE;
    }
    
    Token peek() const {
        if (current < tokens.size()) {
            return tokens[current];
        }
        return Token(TokenType::END_OF_FILE);
    }
    
    Token previous() const {
        if (current > 0) {
            return tokens[current - 1];
        }
        return Token(TokenType::UNKNOWN);
    }
    
    Token advance() {
        if (!isAtEnd()) current++;
        return previous();
    }
    
    bool check(TokenType type) const {
        if (isAtEnd()) return false;
        return peek().type == type;
    }
    
    bool match(TokenType type) {
        if (check(type)) {
            advance();
            return true;
        }
        return false;
    }
    
    bool match(std::initializer_list<TokenType> types) {
        for (TokenType type : types) {
            if (match(type)) return true;
        }
        return false;
    }
    
    void skipNewlines() {
        while (match(TokenType::NEWLINE) || match(TokenType::SEPARATOR)) {}
    }
    
    Token consume(TokenType type, const std::string& message) {
        if (check(type)) return advance();
        throw std::runtime_error(message + " at line " + std::to_string(peek().line));
    }
    
    // Statement parsing
    StmtPtr parseStatement() {
        skipNewlines();
        
        // Variable declaration: 有數x為10。
        if (match(TokenType::KW_LET)) {
            return parseVarDecl();
        }
        
        // Function/class definition: 設術... or 設類...
        if (match(TokenType::KW_DEF)) {
            // Check if it's a class or function
            if (check(TokenType::TYPE_CLASS)) {
                return parseClassDecl();
            } else if (check(TokenType::TYPE_FUNCTION)) {
                return parseFunctionDecl();
            } else if (check(TokenType::KW_UNARY)) {
                return parseUnaryOpOverload();
            } else if (check(TokenType::KW_BINARY)) {
                return parseBinaryOpOverload();
            } else if (check(TokenType::KW_CARGO)) {
                return parseCargoOverload();
            }
            throw std::runtime_error("Expected " +
                                     Keywords::FUNCTION + ", " +
                                     Keywords::CLASS + ", " + 
                                     Keywords::UNARY + ", " +
                                     Keywords::BINARY + ", or" +
                                     Keywords::CARGO + " after " +
                                     Keywords::DEF);
        }
        
        // Import statement: 取「...」
        if (match(TokenType::KW_IMPORT)) {
            return parseImportStmt();
        }
        
        // Print: 曰：...。
        if (match(TokenType::KW_SAY)) {
            return parsePrintStmt();
        }
        
        // Input: 求：x。
        if (match(TokenType::KW_ASK)) {
            return parseInputStmt();
        }
        
        // If: 若...則...
        if (match(TokenType::KW_IF)) {
            return parseIfStmt();
        }
        
        // While: 循...也
        if (match(TokenType::KW_WHILE)) {
            return parseWhileStmt();
        }
        
        // For: 順...也
        if (match(TokenType::KW_FOR)) {
            return parseForStmt();
        }
        
        // Return: 奏...。
        if (match(TokenType::KW_RETURN)) {
            return parseReturnStmt();
        }
        
        // Expression statement or assignment
        return parseExprStatement();
    }
    
    StmtPtr parseVarDecl() {
        // 有數x為10。 or 有數列arr廣10者。
        Token typeToken = advance();
        
        // Check for array: 有數列arr廣...者
        if (check(TokenType::TYPE_ARRAY)) {
            advance(); // consume 列
            return parseArrayDecl(typeToken.lexeme);
        }
        
        // Regular variable
        Token nameToken = consume(TokenType::IDENTIFIER, "Expected variable name");
        
        ExprPtr initializer = nullptr;
        if (match(TokenType::OP_ASSIGN)) {
            initializer = parseExpression();
        }
        
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after variable declaration");
        skipNewlines();
        
        return std::make_unique<VarDeclStmtAST>(typeToken.lexeme, nameToken.lexeme, 
                                                std::move(initializer));
    }
    
    StmtPtr parseArrayDecl(const std::string& elemType) {
        Token nameToken = consume(TokenType::IDENTIFIER, "Expected array name");
        skipNewlines();
        
        consume(TokenType::KW_ARRAY_DECL, "Expected " + Keywords::ARRAY_DECL + " for array declaration");
        
        std::vector<ExprPtr> dimensions;
        dimensions.push_back(parseExpression());
        
        while (match(TokenType::COMMA)) {
            dimensions.push_back(parseExpression());
        }
        
        consume(TokenType::KW_SOLIDIFIER, "Expected " + Keywords::SOLIDIFIER + " after array dimensions");
        skipNewlines();
        
        ExprPtr initializer = nullptr;
        if (match(TokenType::OP_ASSIGN)) {
            initializer = parseExpression();
        }
        
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after array declaration");
        skipNewlines();
        
        return std::make_unique<ArrayDeclStmtAST>(elemType, nameToken.lexeme, 
                                                  std::move(dimensions), std::move(initializer));
    }
    
    StmtPtr parseFunctionDecl() {
        // 設術name，參x、y者，得數
        consume(TokenType::TYPE_FUNCTION, "Expected " + Keywords::FUNCTION + " after " + Keywords::DEF);
        Token nameToken = consume(TokenType::IDENTIFIER, "Expected function name");
        skipNewlines();
        
        std::vector<std::pair<std::string, std::string>> params;
        
        if (match(TokenType::KW_PARAM)) {
            // Parse parameters
            do {
                Token typeToken = advance();
                Token paramName = consume(TokenType::IDENTIFIER, "Expected parameter name");
                params.push_back({typeToken.lexeme, paramName.lexeme});
            } while (match(TokenType::COMMA));
            
            consume(TokenType::KW_SOLIDIFIER, "Expected " + Keywords::SOLIDIFIER + " after parameters");
        }
        
        skipNewlines();
        
        std::string returnType = "";
        if (match(TokenType::KW_YIELD)) {
            Token retType = advance();
            returnType = retType.lexeme;
        }
        
        skipNewlines();
        
        // Parse function body
        std::vector<StmtPtr> body = parseBlock();
        
        return std::make_unique<FunctionDeclAST>(nameToken.lexeme, std::move(params), 
                                                 returnType, std::move(body));
    }
    
    StmtPtr parseClassDecl() {
        // 設類name or 設類name承base
        consume(TokenType::TYPE_CLASS, "Expected " + Keywords::CLASS + " after " + Keywords::DEF);
        Token nameToken = consume(TokenType::IDENTIFIER, "Expected class name");
        
        std::string baseClass = "";
        if (match(TokenType::KW_INHERIT)) {
            Token baseToken = consume(TokenType::IDENTIFIER, "Expected base class name");
            baseClass = baseToken.lexeme;
        }
        
        skipNewlines();
        
        // Parse class members
        std::vector<StmtPtr> members = parseBlock();
        
        return std::make_unique<ClassDeclAST>(nameToken.lexeme, baseClass, std::move(members));
    }
    
    StmtPtr parseUnaryOpOverload() {
        // 設單增
        consume(TokenType::KW_UNARY, "Expected " + Keywords::UNARY);
        Token opToken = advance();  // Get the operator
        
        skipNewlines();
        std::vector<StmtPtr> body = parseBlock();
        
        return std::make_unique<UnaryOpOverloadAST>(opToken.lexeme, std::move(body));
    }
    
    StmtPtr parseBinaryOpOverload() {
        // 設雙益，參人他者
        consume(TokenType::KW_BINARY, "Expected " + Keywords::BINARY);
        Token opToken = advance();  // Get the operator
        skipNewlines();
        
        std::string paramType = "";
        std::string paramName = "";
        
        if (match(TokenType::KW_PARAM)) {
            Token typeToken = advance();
            Token nameToken = consume(TokenType::IDENTIFIER, "Expected parameter name");
            paramType = typeToken.lexeme;
            paramName = nameToken.lexeme;
            consume(TokenType::KW_SOLIDIFIER, "Expected " + Keywords::SOLIDIFIER + " after parameter");
        }
        
        skipNewlines();
        std::vector<StmtPtr> body = parseBlock();
        
        return std::make_unique<BinaryOpOverloadAST>(opToken.lexeme, paramType, 
                                                     paramName, std::move(body));
    }

    StmtPtr parseCargoOverload() {
        // 設載
        consume(TokenType::KW_CARGO, "Expected " + Keywords::CARGO);
        Token opToken = advance();  // Get the operator
        skipNewlines();
        
        std::vector<StmtPtr> body = parseBlock();
        
        return std::make_unique<CargoOpOverloadAST>(opToken.lexeme, std::move(body));
    }
    
    StmtPtr parseImportStmt() {
        // 取「module_path」
        if (match(TokenType::LSTRING1)) {
            // Already consumed 「
        } else if (match(TokenType::LSTRING2)) {
            // Already consumed 『
        } else if (check(TokenType::STRING)) {
            Token moduleToken = advance();
            consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after import statement");
            skipNewlines();
            return std::make_unique<ImportStmtAST>(moduleToken.stringValue);
        } else {
            throw std::runtime_error("Expected string after " + Keywords::IMPORT);
        }
        
        // Parse string content
        std::string modulePath;
        while (!check(TokenType::RSTRING1) && !check(TokenType::RSTRING2) && !isAtEnd()) {
            Token token = advance();
            modulePath += token.lexeme;
        }
        
        if (match(TokenType::RSTRING1) || match(TokenType::RSTRING2)) {
            // Closing quote
        }
        
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after import statement");
        skipNewlines();
        
        return std::make_unique<ImportStmtAST>(modulePath);
    }
    
    StmtPtr parsePrintStmt() {
        // 曰：expr。 with support for stacked colons: 曰：expr：expr：expr。
        // Also continue supporting comma/， separators.
        std::vector<ExprPtr> expressions;
        
        // Require at least one colon-initiated expression
        consume(TokenType::COLON, "Expected " + Keywords::COLON + " after " + Keywords::SAY);
        expressions.push_back(parseExpression());
        
        while (true) {
            if (match(TokenType::COLON)) {
                expressions.push_back(parseExpression());
                continue;
            }
            if (match(TokenType::COMMA) || match(TokenType::SEPARATOR)) {
                expressions.push_back(parseExpression());
                continue;
            }
            break;
        }
        
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after print statement");
        skipNewlines();
        
        return std::make_unique<PrintStmtAST>(std::move(expressions));
    }
    
    StmtPtr parseInputStmt() {
        // 求：x。
        consume(TokenType::COLON, "Expected " + Keywords::COLON + " after " + Keywords::ASK);
        Token varToken = consume(TokenType::IDENTIFIER, "Expected variable name");
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after input statement");
        skipNewlines();
        
        return std::make_unique<InputStmtAST>(varToken.lexeme);
    }
    
    StmtPtr parseIfStmt() {
        // 若condition則body
        ExprPtr condition = parseExpression();
        skipNewlines();
        consume(TokenType::KW_THEN, "Expected " + Keywords::THEN + " after if condition");
        skipNewlines();
        
        std::vector<StmtPtr> thenBranch = parseBlock();
        std::vector<StmtPtr> elseBranch;
        
        // Handle else if: 而condition則
        if (match(TokenType::KW_ELSE_IF)) {
            // This is 而condition則 (else if) - parse as nested if
            auto nestedIf = parseIfStmt();
            elseBranch.push_back(std::move(nestedIf));
        }
        // Handle else: 然
        else if (match(TokenType::KW_ELSE)) {
            skipNewlines();
            consume(TokenType::KW_THEN, "Expected " + Keywords::THEN + " after " + Keywords::ELSE);
            skipNewlines();
            elseBranch = parseBlock();
        }
        
        return std::make_unique<IfStmtAST>(std::move(condition), std::move(thenBranch), 
                                          std::move(elseBranch));
    }
    
    StmtPtr parseWhileStmt() {
        // 循condition也body
        ExprPtr condition = parseExpression();
        consume(TokenType::KW_LOOPER, "Expected " + Keywords::LOOPER + " after while condition");
        skipNewlines();
        
        std::vector<StmtPtr> body = parseBlock();
        
        return std::make_unique<WhileStmtAST>(std::move(condition), std::move(body));
    }
    
    StmtPtr parseForStmt() {
        // Supports two forms:
        // 1) 順數i為0漸一至十也
        // 2) 順i以i為i益一至i如十也  (general update and condition)
        
        // Optional leading type keyword
        std::string varName;
        if (check(TokenType::TYPE_INT) || check(TokenType::TYPE_FLOAT) || check(TokenType::TYPE_BOOL) ||
            check(TokenType::TYPE_STRING) || check(TokenType::TYPE_ARRAY) || check(TokenType::TYPE_TENSOR) ||
            check(TokenType::TYPE_CLASS) || check(TokenType::TYPE_TYPE) || check(TokenType::TYPE_FUNCTION) ||
            check(TokenType::TYPE_OBJECT)) {
            advance(); // consume type token; type is not used yet in codegen
        }
        
        Token varToken = consume(TokenType::IDENTIFIER, "Expected loop variable");
        varName = varToken.lexeme;
        
        consume(TokenType::OP_ASSIGN, "Expected " + Keywords::ASSIGN + " after loop variable");
        ExprPtr start = parseExpression();
        
        // Branch on either '漸' or '以'
        if (match(TokenType::KW_GRADUAL)) {
            // Counting form: 漸 <step> 至 <end>
            ExprPtr step = parseExpression();
            consume(TokenType::KW_TO, "Expected " + Keywords::TO + " in for loop");
            ExprPtr end = parseExpression();
            consume(TokenType::KW_LOOPER, "Expected " + Keywords::LOOPER + " after for loop header");
            skipNewlines();
            std::vector<StmtPtr> body = parseBlock();
            return std::make_unique<ForStmtAST>(varName, std::move(start), std::move(step), std::move(end), std::move(body));
        } else {
            // Using form: 以 <x> 為 <expr1> 至 <expr2> 也
            consume(TokenType::KW_USING, "Expected " + Keywords::GRADUAL + " or " + Keywords::USING + " in for loop");
            // The grammar in docs shows '以<x>為<expr1>至<expr2>' where <x> repeats the var
            // Accept either the same identifier or skip if omitted
            if (check(TokenType::IDENTIFIER)) {
                Token repeatVar = advance();
                (void)repeatVar; // ignore name; we assume same var
            }
            consume(TokenType::OP_ASSIGN, "Expected " + Keywords::ASSIGN + " in using-form for loop");
            ExprPtr update = parseExpression();
            consume(TokenType::KW_TO, "Expected " + Keywords::TO + " in using-form for loop");
            ExprPtr condition = parseExpression();
            consume(TokenType::KW_LOOPER, "Expected " + Keywords::LOOPER + " after for loop header");
            skipNewlines();
            std::vector<StmtPtr> body = parseBlock();
            return std::make_unique<ForStmtAST>(varName, std::move(start), std::move(update), std::move(condition), std::move(body), true);
        }
    }
    
    StmtPtr parseReturnStmt() {
        ExprPtr value = nullptr;
        if (!check(TokenType::PERIOD)) {
            value = parseExpression();
        }
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after return statement");
        skipNewlines();
        
        return std::make_unique<ReturnStmtAST>(std::move(value));
    }
    
    StmtPtr parseExprStatement() {
        // Could be assignment or just an expression
        ExprPtr expr = parseExpression();
        
        // Check for assignment
        if (match(TokenType::OP_ASSIGN)) {
            // Check if it's a variable or array element assignment
            auto* varExpr = dynamic_cast<VariableExprAST*>(expr.get());
            auto* indexExpr = dynamic_cast<IndexExprAST*>(expr.get());
            
            ExprPtr value = parseExpression();
            consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after assignment");
            skipNewlines();
            
            if (varExpr) {
                // Simple variable assignment
                std::string varName = varExpr->name;
                expr.release(); // Release ownership
                return std::make_unique<AssignStmtAST>(varName, std::move(value));
            } else if (indexExpr) {
                // Array element assignment
                std::string arrayName = indexExpr->arrayName;
                std::vector<ExprPtr> indices;
                for (auto& idx : indexExpr->indices) {
                    // We need to clone the indices - for now just move them
                    indices.push_back(std::move(idx));
                }
                expr.release(); // Release ownership
                return std::make_unique<ArrayAssignStmtAST>(arrayName, std::move(indices), std::move(value));
            } else {
                throw std::runtime_error("Invalid assignment target");
            }
        }
        
        consume(TokenType::PERIOD, "Expected " + Keywords::PERIOD + " after expression");
        skipNewlines();
        
        return std::make_unique<ExprStmtAST>(std::move(expr));
    }
    
    std::vector<StmtPtr> parseBlock() {
        std::vector<StmtPtr> statements;
        
        // Check for explicit grouping 《...》
        if (match(TokenType::LGROUP)) {
            while (!check(TokenType::RGROUP) && !isAtEnd()) {
                if (match(TokenType::NEWLINE)) continue;
                auto stmt = parseStatement();
                if (stmt) statements.push_back(std::move(stmt));
            }
            consume(TokenType::RGROUP, "Expected " + Keywords::RGROUP + " to close block");
            skipNewlines();
            return statements;
        }
        
        // Otherwise, use indentation
        bool hasIndent = match(TokenType::INDENT);
        
        do {
            if (match(TokenType::NEWLINE)) continue;
            if (check(TokenType::DEDENT) || isAtEnd()) break;
            
            auto stmt = parseStatement();
            if (stmt) statements.push_back(std::move(stmt));
            
        } while (hasIndent && !check(TokenType::DEDENT));
        
        if (hasIndent) {
            match(TokenType::DEDENT);
        }
        
        return statements;
    }
    
    // Expression parsing with operator precedence
    ExprPtr parseExpression() {
        return parseAssignment();
    }
    
    ExprPtr parseAssignment() {
        return parseEquality();
    }
    
    ExprPtr parseEquality() {
        ExprPtr expr = parseComparison();
        
        while (match({TokenType::OP_EQ, TokenType::OP_NEQ})) {
            Token op = previous();
            ExprPtr right = parseComparison();
            expr = std::make_unique<BinaryExprAST>(op.lexeme, std::move(expr), std::move(right));
        }
        
        return expr;
    }
    
    ExprPtr parseComparison() {
        ExprPtr expr = parseAddition();
        
        while (match({TokenType::OP_GT, TokenType::OP_LT, TokenType::OP_GTE, TokenType::OP_LTE})) {
            Token op = previous();
            ExprPtr right = parseAddition();
            expr = std::make_unique<BinaryExprAST>(op.lexeme, std::move(expr), std::move(right));
        }
        
        return expr;
    }
    
    ExprPtr parseAddition() {
        ExprPtr expr = parseMultiplication();
        
        while (match({TokenType::OP_ADD, TokenType::OP_SUB})) {
            Token op = previous();
            ExprPtr right = parseMultiplication();
            expr = std::make_unique<BinaryExprAST>(op.lexeme, std::move(expr), std::move(right));
        }
        
        return expr;
    }
    
    ExprPtr parseMultiplication() {
        ExprPtr expr = parsePower();
        
        while (match({TokenType::OP_MUL, TokenType::OP_DIV, TokenType::OP_FLOORDIV, TokenType::OP_MOD})) {
            Token op = previous();
            ExprPtr right = parsePower();
            expr = std::make_unique<BinaryExprAST>(op.lexeme, std::move(expr), std::move(right));
        }
        
        return expr;
    }
    
    ExprPtr parsePower() {
        ExprPtr expr = parsePostfix();
        
        if (match(TokenType::OP_POW)) {
            Token op = previous();
            ExprPtr right = parsePower(); // Right associative
            expr = std::make_unique<BinaryExprAST>(op.lexeme, std::move(expr), std::move(right));
        }
        
        return expr;
    }
    
    ExprPtr parsePostfix() {
        ExprPtr expr = parseUnary();
        
        while (true) {
            // Member access: x之y
            if (match(TokenType::KW_ACCESS)) {
                Token member = consume(TokenType::IDENTIFIER, "Expected member name after 之");
                expr = std::make_unique<MemberAccessExprAST>(std::move(expr), member.lexeme);
            }
            // Array indexing: arr其0者
            else if (match(TokenType::KW_INDEX)) {
                std::vector<ExprPtr> indices;
                indices.push_back(parseExpression());
                
                while (match(TokenType::COMMA)) {
                    indices.push_back(parseExpression());
                }
                
                consume(TokenType::KW_SOLIDIFIER, "Expected " + Keywords::SOLIDIFIER + " after array index");
                
                // Get array name
                auto* varExpr = dynamic_cast<VariableExprAST*>(expr.get());
                if (!varExpr) {
                    throw std::runtime_error("Invalid array indexing");
                }
                std::string arrayName = varExpr->name;
                expr.release();
                
                expr = std::make_unique<IndexExprAST>(arrayName, std::move(indices));
            }
            // Function call: func執arg1、arg2者
            else if (match(TokenType::KW_CALL)) {
                std::vector<ExprPtr> args;
                
                if (!check(TokenType::KW_SOLIDIFIER)) {
                    args.push_back(parseExpression());
                    
                    while (match(TokenType::COMMA)) {
                        args.push_back(parseExpression());
                    }
                }
                
                consume(TokenType::KW_SOLIDIFIER, "Expected " + Keywords::SOLIDIFIER + " after function arguments");
                
                // Get function name
                auto* varExpr = dynamic_cast<VariableExprAST*>(expr.get());
                if (!varExpr) {
                    throw std::runtime_error("Invalid function call");
                }
                std::string funcName = varExpr->name;
                expr.release();
                
                expr = std::make_unique<CallExprAST>(funcName, std::move(args));
            }
            // Increment/decrement
            else if (match({TokenType::OP_INC, TokenType::OP_DEC})) {
                Token op = previous();
                expr = std::make_unique<UnaryExprAST>(op.lexeme, std::move(expr));
            }
            else {
                break;
            }
        }
        
        return expr;
    }

    ExprPtr parseUnary() {
        // Handle unary 負 or ASCII '-'
        if (match(TokenType::OP_NEG) || (check(TokenType::OP_SUB) && (previous().type != TokenType::INTEGER && previous().type != TokenType::FLOAT && previous().type != TokenType::IDENTIFIER))) {
            // Consume '-' if it's there
            if (check(TokenType::OP_SUB)) advance();
            ExprPtr right = parseUnary();
            // Represent as 0 - right (binary), to reuse codegen
            ExprPtr zero = std::make_unique<IntegerExprAST>(0);
            return std::make_unique<BinaryExprAST>(Keywords::SUB, std::move(zero), std::move(right));
        }
        return parsePrimary();
    }

    ExprPtr parsePrimary() {
        // Literals
        if (match(TokenType::INTEGER)) {
            return std::make_unique<IntegerExprAST>(previous().intValue);
        }
        
        if (match(TokenType::FLOAT)) {
            return std::make_unique<FloatExprAST>(previous().floatValue);
        }
        
        if (match(TokenType::STRING)) {
            return std::make_unique<StringExprAST>(previous().stringValue);
        }
        // Special literal: 更 -> "\n"
        if (match(TokenType::LIT_NEWLINE)) {
            return std::make_unique<StringExprAST>("\n");
        }
        
        if (match({TokenType::BOOL_TRUE, TokenType::BOOL_FALSE})) {
            return std::make_unique<BoolExprAST>(previous().type == TokenType::BOOL_TRUE);
        }
        
        // Special case: 增 (increment) as standalone means +1, 削 (decrement) means -1
        if (match(TokenType::OP_INC)) {
            return std::make_unique<IntegerExprAST>(1);
        }
        
        if (match(TokenType::OP_DEC)) {
            return std::make_unique<IntegerExprAST>(-1);
        }
        
        // Identifiers
        if (match(TokenType::IDENTIFIER)) {
            return std::make_unique<VariableExprAST>(previous().lexeme);
        }
        
        // Grouped expression: 《expr》
        if (match(TokenType::LGROUP)) {
            ExprPtr expr = parseExpression();
            consume(TokenType::RGROUP, "Expected " + Keywords::RGROUP + " after grouped expression");
            return expr;
        }
        
        // Parenthesized expression
        if (match(TokenType::LPAREN)) {
            ExprPtr expr = parseExpression();
            consume(TokenType::RPAREN, "Expected " + Keywords::RPAREN + " after expression");
            return expr;
        }
        
        throw std::runtime_error("Unexpected token: " + peek().lexeme + 
                               " at line " + std::to_string(peek().line));
    }
};

} // namespace wuyu

