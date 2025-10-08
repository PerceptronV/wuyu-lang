#pragma once

#include <memory>
#include <vector>
#include <string>
#include <variant>
#include "keywords.h"

namespace wuyu {

// Forward declarations
class ExprAST;
class StmtAST;

using ExprPtr = std::unique_ptr<ExprAST>;
using StmtPtr = std::unique_ptr<StmtAST>;

// Base class for all expression nodes
class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual std::string toString() const = 0;
};

// Base class for all statement nodes
class StmtAST {
public:
    virtual ~StmtAST() = default;
    virtual std::string toString() const = 0;
};

// Literal expressions
class IntegerExprAST : public ExprAST {
public:
    int64_t value;
    IntegerExprAST(int64_t value) : value(value) {}
    std::string toString() const override { return std::to_string(value); }
};

class FloatExprAST : public ExprAST {
public:
    double value;
    FloatExprAST(double value) : value(value) {}
    std::string toString() const override { return std::to_string(value); }
};

class StringExprAST : public ExprAST {
public:
    std::string value;
    StringExprAST(const std::string& value) : value(value) {}
    std::string toString() const override { return Keywords::LSTRING1 + value + Keywords::RSTRING1; }
};

class BoolExprAST : public ExprAST {
public:
    bool value;
    BoolExprAST(bool value) : value(value) {}
    std::string toString() const override { return value ? Keywords::TRUE : Keywords::FALSE; }
};

// Variable reference
class VariableExprAST : public ExprAST {
public:
    std::string name;
    VariableExprAST(const std::string& name) : name(name) {}
    std::string toString() const override { return name; }
};

// Binary operation
class BinaryExprAST : public ExprAST {
public:
    std::string op;
    ExprPtr lhs;
    ExprPtr rhs;
    
    BinaryExprAST(const std::string& op, ExprPtr lhs, ExprPtr rhs)
        : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    
    std::string toString() const override {
        return Keywords::LPAREN + lhs->toString() + " " + op + " " + rhs->toString() + Keywords::RPAREN;
    }
};

// Unary operation
class UnaryExprAST : public ExprAST {
public:
    std::string op;
    ExprPtr operand;
    
    UnaryExprAST(const std::string& op, ExprPtr operand)
        : op(op), operand(std::move(operand)) {}
    
    std::string toString() const override {
        return Keywords::LPAREN + op + " " + operand->toString() + Keywords::RPAREN;
    }
};

// Cargo operation
class CargoExprAST : public ExprAST {
public:
    std::string op;
    ExprPtr operand;

    CargoExprAST(const std::string& op, ExprPtr operand)
        : op(op), operand(std::move(operand)) {}
    
    std::string toString() const override {
        return Keywords::LPAREN + op + " " + operand->toString() + Keywords::RPAREN;
    }
};

// Function call
class CallExprAST : public ExprAST {
public:
    std::string callee;
    std::vector<ExprPtr> args;
    
    CallExprAST(const std::string& callee, std::vector<ExprPtr> args)
        : callee(callee), args(std::move(args)) {}
    
    std::string toString() const override {
        std::string result = callee + Keywords::CALL;
        for (size_t i = 0; i < args.size(); i++) {
            if (i > 0) result += Keywords::COMMA;
            result += args[i]->toString();
        }
        result += Keywords::SOLIDIFIER;
        return result;
    }
};

// Array indexing
class IndexExprAST : public ExprAST {
public:
    std::string arrayName;
    std::vector<ExprPtr> indices;
    
    IndexExprAST(const std::string& arrayName, std::vector<ExprPtr> indices)
        : arrayName(arrayName), indices(std::move(indices)) {}
    
    std::string toString() const override {
        std::string result = arrayName + Keywords::INDEX;
        for (size_t i = 0; i < indices.size(); i++) {
            if (i > 0) result += Keywords::COMMA;
            result += indices[i]->toString();
        }
        result += Keywords::SOLIDIFIER;
        return result;
    }
};

// Member access (之)
class MemberAccessExprAST : public ExprAST {
public:
    ExprPtr object;
    std::string member;
    
    MemberAccessExprAST(ExprPtr object, const std::string& member)
        : object(std::move(object)), member(member) {}
    
    std::string toString() const override {
        return object->toString() + Keywords::ACCESS + member;
    }
};

// Variable declaration statement
class VarDeclStmtAST : public StmtAST {
public:
    std::string type;  // 數、分、文、etc.
    std::string name;
    ExprPtr initializer;
    
    VarDeclStmtAST(const std::string& type, const std::string& name, ExprPtr initializer = nullptr)
        : type(type), name(name), initializer(std::move(initializer)) {}
    
    std::string toString() const override {
        std::string result = Keywords::LET + type + name;
        if (initializer) {
            result += Keywords::ASSIGN + initializer->toString();
        }
        return result + Keywords::PERIOD;
    }
};

// Array declaration
class ArrayDeclStmtAST : public StmtAST {
public:
    std::string type;
    std::string name;
    std::vector<ExprPtr> dimensions;
    ExprPtr initializer;
    
    ArrayDeclStmtAST(const std::string& type, const std::string& name, 
                     std::vector<ExprPtr> dimensions, ExprPtr initializer = nullptr)
        : type(type), name(name), dimensions(std::move(dimensions)), 
          initializer(std::move(initializer)) {}
    
    std::string toString() const override {
        std::string result = Keywords::LET + type + Keywords::ARRAY + name + Keywords::ARRAY_DECL;
        for (size_t i = 0; i < dimensions.size(); i++) {
            if (i > 0) result += Keywords::COMMA;
            result += dimensions[i]->toString();
        }
        result += Keywords::SOLIDIFIER;
        return result;
    }
};

// Assignment statement
class AssignStmtAST : public StmtAST {
public:
    std::string varName;
    ExprPtr value;
    
    AssignStmtAST(const std::string& varName, ExprPtr value)
        : varName(varName), value(std::move(value)) {}
    
    std::string toString() const override {
        return varName + Keywords::ASSIGN + value->toString() + Keywords::PERIOD;
    }
};

// Array element assignment: arr其0者為value。
class ArrayAssignStmtAST : public StmtAST {
public:
    std::string arrayName;
    std::vector<ExprPtr> indices;
    ExprPtr value;
    
    ArrayAssignStmtAST(const std::string& arrayName, std::vector<ExprPtr> indices, ExprPtr value)
        : arrayName(arrayName), indices(std::move(indices)), value(std::move(value)) {}
    
    std::string toString() const override {
        return arrayName + Keywords::INDEX + "..." + Keywords::SOLIDIFIER + Keywords::ASSIGN + value->toString() + Keywords::PERIOD;
    }
};

// Expression statement
class ExprStmtAST : public StmtAST {
public:
    ExprPtr expr;
    
    ExprStmtAST(ExprPtr expr) : expr(std::move(expr)) {}
    
    std::string toString() const override {
        return expr->toString() + Keywords::PERIOD;
    }
};

// Print statement (曰)
class PrintStmtAST : public StmtAST {
public:
    std::vector<ExprPtr> expressions;
    
    PrintStmtAST(std::vector<ExprPtr> expressions)
        : expressions(std::move(expressions)) {}
    
    std::string toString() const override {
        std::string result = Keywords::SAY + Keywords::COLON;
        for (const auto& expr : expressions) {
            result += expr->toString();
        }
        return result + Keywords::PERIOD;
    }
};

// Input statement (求)
class InputStmtAST : public StmtAST {
public:
    std::string varName;
    
    InputStmtAST(const std::string& varName) : varName(varName) {}
    
    std::string toString() const override {
        return Keywords::ASK + Keywords::COLON + varName + Keywords::PERIOD;
    }
};

// If statement
class IfStmtAST : public StmtAST {
public:
    ExprPtr condition;
    std::vector<StmtPtr> thenBranch;
    std::vector<StmtPtr> elseBranch;
    
    IfStmtAST(ExprPtr condition, std::vector<StmtPtr> thenBranch, 
              std::vector<StmtPtr> elseBranch = {})
        : condition(std::move(condition)), thenBranch(std::move(thenBranch)), 
          elseBranch(std::move(elseBranch)) {}
    
    std::string toString() const override {
        return Keywords::IF + condition->toString() + Keywords::THEN + "...";
    }
};

// While loop
class WhileStmtAST : public StmtAST {
public:
    ExprPtr condition;
    std::vector<StmtPtr> body;
    
    WhileStmtAST(ExprPtr condition, std::vector<StmtPtr> body)
        : condition(std::move(condition)), body(std::move(body)) {}
    
    std::string toString() const override {
        return Keywords::WHILE + condition->toString() + Keywords::LOOPER + "...";
    }
};

// For loop
class ForStmtAST : public StmtAST {
public:
    std::string varName;
    ExprPtr start;
    // Counting-for: step and end are used when isUsingForm == false
    ExprPtr step;
    ExprPtr end;
    // Using-form: custom update and loop condition when isUsingForm == true
    bool isUsingForm = false;
    ExprPtr update;      // expression assigned to var each iteration
    ExprPtr condition;   // loop-continue condition
    std::vector<StmtPtr> body;
    
    // Counting-form constructor
    ForStmtAST(const std::string& varName, ExprPtr start, ExprPtr step, 
               ExprPtr end, std::vector<StmtPtr> body)
        : varName(varName), start(std::move(start)), step(std::move(step)), 
          end(std::move(end)), body(std::move(body)) {}

    // Using-form constructor
    ForStmtAST(const std::string& varName, ExprPtr start, ExprPtr update,
               ExprPtr condition, std::vector<StmtPtr> body, bool usingForm)
        : varName(varName), start(std::move(start)), isUsingForm(usingForm), 
          update(std::move(update)), condition(std::move(condition)), body(std::move(body)) {}
    
    std::string toString() const override {
        if (!isUsingForm) {
            return Keywords::FOR + Keywords::INT + varName + Keywords::ASSIGN + start->toString() + Keywords::GRADUAL + 
                   step->toString() + Keywords::TO + end->toString() + Keywords::LOOPER + "...";
        }
        return Keywords::FOR + varName + Keywords::USING + varName + Keywords::ASSIGN + update->toString() +
               Keywords::TO + condition->toString() + Keywords::LOOPER + "...";
    }
};

// Function declaration
class FunctionDeclAST : public StmtAST {
public:
    std::string name;
    std::vector<std::pair<std::string, std::string>> params; // (type, name)
    std::string returnType;
    std::vector<StmtPtr> body;
    
    FunctionDeclAST(const std::string& name, 
                    std::vector<std::pair<std::string, std::string>> params,
                    const std::string& returnType,
                    std::vector<StmtPtr> body)
        : name(name), params(std::move(params)), returnType(returnType), 
          body(std::move(body)) {}
    
    std::string toString() const override {
        return Keywords::DEF + Keywords::FUNCTION + name + "...";
    }
};

// Return statement
class ReturnStmtAST : public StmtAST {
public:
    ExprPtr value;
    
    ReturnStmtAST(ExprPtr value = nullptr) : value(std::move(value)) {}
    
    std::string toString() const override {
        if (value) return Keywords::RETURN + value->toString() + Keywords::PERIOD;
        return Keywords::RETURN + Keywords::PERIOD;
    }
};

// Block/compound statement
class BlockStmtAST : public StmtAST {
public:
    std::vector<StmtPtr> statements;
    
    BlockStmtAST(std::vector<StmtPtr> statements)
        : statements(std::move(statements)) {}
    
    std::string toString() const override {
        return Keywords::LGROUP + "..." + Keywords::RGROUP;
    }
};

// Class declaration
class ClassDeclAST : public StmtAST {
public:
    std::string name;
    std::string baseClass;  // empty if no inheritance
    std::vector<StmtPtr> members;  // methods and field declarations
    
    ClassDeclAST(const std::string& name, const std::string& baseClass,
                 std::vector<StmtPtr> members)
        : name(name), baseClass(baseClass), members(std::move(members)) {}
    
    std::string toString() const override {
        std::string result = Keywords::DEF + Keywords::CLASS + name;
        if (!baseClass.empty()) {
            result += Keywords::INHERIT + baseClass;
        }
        return result + "...";
    }
};

// Method declaration (within a class)
class MethodDeclAST : public StmtAST {
public:
    std::string name;
    std::vector<std::pair<std::string, std::string>> params;
    std::string returnType;
    std::vector<StmtPtr> body;
    bool isConstructor;
    
    MethodDeclAST(const std::string& name,
                  std::vector<std::pair<std::string, std::string>> params,
                  const std::string& returnType,
                  std::vector<StmtPtr> body,
                  bool isConstructor = false)
        : name(name), params(std::move(params)), returnType(returnType),
          body(std::move(body)), isConstructor(isConstructor) {}
    
    std::string toString() const override {
        return Keywords::DEF + Keywords::FUNCTION + name + "...";
    }
};

// Unary operator overload
class UnaryOpOverloadAST : public StmtAST {
public:
    std::string op;
    std::vector<StmtPtr> body;
    
    UnaryOpOverloadAST(const std::string& op, std::vector<StmtPtr> body)
        : op(op), body(std::move(body)) {}
    
    std::string toString() const override {
        return Keywords::DEF + Keywords::FUNCTION + op + "...";
    }
};

// Binary operator overload
class BinaryOpOverloadAST : public StmtAST {
public:
    std::string op;
    std::string paramType;
    std::string paramName;
    std::vector<StmtPtr> body;
    
    BinaryOpOverloadAST(const std::string& op, const std::string& paramType,
                       const std::string& paramName, std::vector<StmtPtr> body)
        : op(op), paramType(paramType), paramName(paramName), 
          body(std::move(body)) {}
    
    std::string toString() const override {
        return Keywords::DEF + Keywords::FUNCTION + op + "...";
    }
};

// Cargo operator overload
class CargoOpOverloadAST : public StmtAST {
public:
    std::string op;
    std::vector<StmtPtr> body;
    
    CargoOpOverloadAST(const std::string& op, std::vector<StmtPtr> body)
        : op(op), body(std::move(body)) {}
    
    std::string toString() const override {
        return Keywords::DEF + Keywords::FUNCTION + op + "...";
    }
};

// Import statement
class ImportStmtAST : public StmtAST {
public:
    std::string modulePath;
    
    ImportStmtAST(const std::string& modulePath) : modulePath(modulePath) {}
    
    std::string toString() const override {
        return Keywords::IMPORT + Keywords::LSTRING1 + modulePath + Keywords::RSTRING1;
    }
};

// Program (root node)
class ProgramAST {
public:
    std::vector<StmtPtr> statements;
    
    ProgramAST(std::vector<StmtPtr> statements)
        : statements(std::move(statements)) {}
    
    std::string toString() const {
        std::string result;
        for (const auto& stmt : statements) {
            result += stmt->toString() + "\n";
        }
        return result;
    }
};

} // namespace wuyu

