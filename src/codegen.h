#pragma once

#include "ast.h"
#include "keywords.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <string>
#include <memory>
#include <iostream>

namespace wuyu {

// Forward declaration of runtime helpers provided by host
extern "C" long wuyu_parse_int_utf8(const char*);
extern "C" double wuyu_parse_float_utf8(const char*);

class CodeGenerator {
private:
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    
    // Symbol table: variable name -> LLVM value
    std::map<std::string, llvm::AllocaInst*> namedValues;
    
    // Class definitions: class name -> struct type
    std::map<std::string, llvm::StructType*> classTypes;
    
    // Class methods: class_name.method_name -> function
    std::map<std::string, llvm::Function*> classMethods;
    
    // Class inheritance: class_name -> base_class_name
    std::map<std::string, std::string> classInheritance;
    
    // Current function being generated
    llvm::Function* currentFunction;
    
    // Current class being generated (for 己/self)
    std::string currentClass;
    
    // REPL/session mode support: generate globals and snippet functions
    bool sessionMode = false;
    int snippetCounter = 0;
    // Track global variable types across snippets (by name)
    std::map<std::string, std::string> replGlobalTypeNames;
    // Track function signatures across snippets (name -> (paramTypes, returnType))
    std::map<std::string, std::pair<std::vector<std::string>, std::string>> replFunctionSignatures;
    // Name of the generated entry function ("main" or snippet)
    std::string currentEntryFunctionName;
    // Custom entry function name (if set, overrides default naming)
    std::string customEntryName;
    
public:
    CodeGenerator(const std::string& moduleName = "wuyu_module") 
        : context(std::make_unique<llvm::LLVMContext>()),
          module(std::make_unique<llvm::Module>(moduleName, *context)),
          builder(std::make_unique<llvm::IRBuilder<>>(*context)),
          currentFunction(nullptr) {
        
        // Declare external functions (printf, scanf, etc.)
        declareExternalFunctions();
    }
    
    llvm::LLVMContext& getContext() { return *context; }
    llvm::Module& getModule() { return *module; }
    std::unique_ptr<llvm::Module> releaseModule() { return std::move(module); }
    
    void setSessionMode(bool enabled) { sessionMode = enabled; }
    void setCustomEntryName(const std::string& name) { customEntryName = name; }
    void importReplTypeMap(const std::map<std::string, std::string>& m) { replGlobalTypeNames = m; }
    std::map<std::string, std::string> exportReplTypeMap() const { return replGlobalTypeNames; }
    void importReplFunctionSignatures(const std::map<std::string, std::pair<std::vector<std::string>, std::string>>& sigs) { 
        replFunctionSignatures = sigs; 
    }
    std::map<std::string, std::pair<std::vector<std::string>, std::string>> exportReplFunctionSignatures() const { 
        return replFunctionSignatures; 
    }
    
    void generate(const ProgramAST& program) {
        // Create entry function depending on mode
        llvm::Function* entryFunc;
        if (!customEntryName.empty()) {
            // Use custom entry name if provided
            llvm::FunctionType* fnType = llvm::FunctionType::get(
                llvm::Type::getInt64Ty(*context), false);
            entryFunc = llvm::Function::Create(
                fnType, llvm::Function::ExternalLinkage, customEntryName, module.get());
            currentEntryFunctionName = customEntryName;
        } else if (!sessionMode) {
            llvm::FunctionType* mainType = llvm::FunctionType::get(
                llvm::Type::getInt64Ty(*context), false);
            entryFunc = llvm::Function::Create(
                mainType, llvm::Function::ExternalLinkage, "main", module.get());
            currentEntryFunctionName = "main";
        } else {
            llvm::FunctionType* fnType = llvm::FunctionType::get(
                llvm::Type::getInt64Ty(*context), false);
            std::string name = "__snippet_" + std::to_string(snippetCounter++);
            entryFunc = llvm::Function::Create(
                fnType, llvm::Function::ExternalLinkage, name, module.get());
            currentEntryFunctionName = name;
        }

        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", entryFunc);
        builder->SetInsertPoint(entry);
        currentFunction = entryFunc;

        // Generate code for all statements
        for (const auto& stmt : program.statements) {
            generateStmt(*stmt);
        }

        // Return 0 if no explicit return
        if (!builder->GetInsertBlock()->getTerminator()) {
            builder->CreateRet(llvm::ConstantInt::get(*context, llvm::APInt(64, 0)));
        }

        // Verify the module
        std::string errorStr;
        llvm::raw_string_ostream errorStream(errorStr);
        if (llvm::verifyModule(*module, &errorStream)) {
            std::cerr << "Module verification failed:\n" << errorStr << std::endl;
        }
    }
    
    void printIR() {
        module->print(llvm::outs(), nullptr);
    }
    
    const std::string& getEntryFunctionName() const { return currentEntryFunctionName; }
    
private:
    void declareExternalFunctions() {
        // printf(char*, ...)
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(*context),
            llvm::PointerType::getUnqual(*context),
            true);
        llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, 
                              "printf", module.get());
        
        // scanf(char*, ...)
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(*context),
            llvm::PointerType::getUnqual(*context),
            true);
        llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage,
                              "scanf", module.get());
        
        // puts(char*)
        llvm::FunctionType* putsType = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(*context),
            llvm::PointerType::getUnqual(*context),
            false);
        llvm::Function::Create(putsType, llvm::Function::ExternalLinkage,
                              "puts", module.get());

    }
    
    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* function,
                                             const std::string& varName,
                                             llvm::Type* type) {
        llvm::IRBuilder<> tmpBuilder(&function->getEntryBlock(),
                                     function->getEntryBlock().begin());
        return tmpBuilder.CreateAlloca(type, nullptr, varName);
    }
    
    llvm::Type* getTypeFromString(const std::string& typeName) {
        if (typeName == Keywords::INT || typeName == "int") {
            return llvm::Type::getInt64Ty(*context);
        } else if (typeName == Keywords::FLOAT || typeName == "float") {
            return llvm::Type::getDoubleTy(*context);
        } else if (typeName == Keywords::BOOL || typeName == "bool") {
            return llvm::Type::getInt1Ty(*context);
        } else if (typeName == Keywords::STRING || typeName == "string") {
            return llvm::PointerType::getUnqual(*context);
        }
        return llvm::Type::getInt64Ty(*context); // Default to int64
    }
    
    void generateStmt(const StmtAST& stmt) {
        if (auto* varDecl = dynamic_cast<const VarDeclStmtAST*>(&stmt)) {
            generateVarDecl(*varDecl);
        } else if (auto* arrayDecl = dynamic_cast<const ArrayDeclStmtAST*>(&stmt)) {
            generateArrayDecl(*arrayDecl);
        } else if (auto* assign = dynamic_cast<const AssignStmtAST*>(&stmt)) {
            generateAssign(*assign);
        } else if (auto* arrayAssign = dynamic_cast<const ArrayAssignStmtAST*>(&stmt)) {
            generateArrayAssign(*arrayAssign);
        } else if (auto* print = dynamic_cast<const PrintStmtAST*>(&stmt)) {
            generatePrint(*print);
        } else if (auto* input = dynamic_cast<const InputStmtAST*>(&stmt)) {
            generateInput(*input);
        } else if (auto* ifStmt = dynamic_cast<const IfStmtAST*>(&stmt)) {
            generateIf(*ifStmt);
        } else if (auto* whileStmt = dynamic_cast<const WhileStmtAST*>(&stmt)) {
            generateWhile(*whileStmt);
        } else if (auto* forStmt = dynamic_cast<const ForStmtAST*>(&stmt)) {
            generateFor(*forStmt);
        } else if (auto* funcDecl = dynamic_cast<const FunctionDeclAST*>(&stmt)) {
            generateFunction(*funcDecl);
        } else if (auto* classDecl = dynamic_cast<const ClassDeclAST*>(&stmt)) {
            generateClass(*classDecl);
        } else if (auto* retStmt = dynamic_cast<const ReturnStmtAST*>(&stmt)) {
            generateReturn(*retStmt);
        } else if (auto* importStmt = dynamic_cast<const ImportStmtAST*>(&stmt)) {
            generateImport(*importStmt);
        } else if (auto* exprStmt = dynamic_cast<const ExprStmtAST*>(&stmt)) {
            generateExpr(*exprStmt->expr);
        }
    }
    
    void generateVarDecl(const VarDeclStmtAST& stmt) {
        llvm::Type* type = getTypeFromString(stmt.type);
        if (!sessionMode) {
            llvm::AllocaInst* alloca = createEntryBlockAlloca(currentFunction, stmt.name, type);
            namedValues[stmt.name] = alloca;
            if (stmt.initializer) {
                llvm::Value* initValue = generateExpr(*stmt.initializer);
                if (initValue) builder->CreateStore(initValue, alloca);
            } else {
                if (type->isIntegerTy(64)) {
                    builder->CreateStore(llvm::ConstantInt::get(type, 0), alloca);
                } else if (type->isDoubleTy()) {
                    builder->CreateStore(llvm::ConstantFP::get(type, 0.0), alloca);
                } else if (type->isIntegerTy(1)) {
                    builder->CreateStore(llvm::ConstantInt::get(type, 0), alloca);
                } else if (type->isPointerTy()) {
                    builder->CreateStore(llvm::ConstantPointerNull::get(
                        llvm::cast<llvm::PointerType>(type)), alloca);
                }
            }
        } else {
            // Define or reuse a global variable for REPL persistence
            llvm::GlobalVariable* gv = module->getGlobalVariable(stmt.name);
            bool isNewVariable = (replGlobalTypeNames.find(stmt.name) == replGlobalTypeNames.end());
            
            if (!gv) {
                if (isNewVariable) {
                    // First time seeing this variable - create a proper definition
                    gv = new llvm::GlobalVariable(
                        *module,
                        type,
                        false,
                        llvm::GlobalValue::ExternalLinkage,
                        llvm::Constant::getNullValue(type),
                        stmt.name);
                } else {
                    // Variable exists from previous module - create external declaration
                    gv = new llvm::GlobalVariable(
                        *module,
                        type,
                        false,
                        llvm::GlobalValue::ExternalLinkage,
                        nullptr,  // No initializer - external declaration
                        stmt.name);
                }
            }
            replGlobalTypeNames[stmt.name] = stmt.type;
            if (stmt.initializer) {
                llvm::Value* initValue = generateExpr(*stmt.initializer);
                if (initValue) builder->CreateStore(initValue, gv);
            }
        }
    }
    
    void generateAssign(const AssignStmtAST& stmt) {
        llvm::Value* value = generateExpr(*stmt.value);
        llvm::AllocaInst* variable = namedValues[stmt.varName];
        if (variable) {
            builder->CreateStore(value, variable);
            return;
        }
            if (sessionMode) {
                auto it = replGlobalTypeNames.find(stmt.varName);
                if (it != replGlobalTypeNames.end()) {
                    llvm::Type* gvType = getTypeFromString(it->second);
                    llvm::GlobalVariable* gv = module->getGlobalVariable(stmt.varName);
                    if (!gv) {
                        // Create external declaration (no initializer)
                        gv = new llvm::GlobalVariable(*module, gvType, false,
                                                      llvm::GlobalValue::ExternalLinkage,
                                                      nullptr,  // External declaration
                                                      stmt.varName);
                    }
                    builder->CreateStore(value, gv);
                    return;
                }
            }
        std::cerr << "Undefined variable: " << stmt.varName << std::endl;
    }
    
    void generateArrayAssign(const ArrayAssignStmtAST& stmt) {
        // Get array pointer
        llvm::AllocaInst* arrayPtr = namedValues[stmt.arrayName];
        if (!arrayPtr) {
            std::cerr << "Undefined array: " << stmt.arrayName << std::endl;
            return;
        }
        
        // For now, support only 1D arrays
        if (stmt.indices.size() != 1) {
            std::cerr << "Multi-dimensional array assignment not yet supported\n";
            return;
        }
        
        // Generate index
        llvm::Value* index = generateExpr(*stmt.indices[0]);
        if (!index) return;
        
        // Generate value to store
        llvm::Value* value = generateExpr(*stmt.value);
        if (!value) return;
        
        // Get element pointer
        llvm::Value* elemPtr = builder->CreateGEP(
            arrayPtr->getAllocatedType(), arrayPtr, index, "arrayidx");
        
        // Store the value
        builder->CreateStore(value, elemPtr);
    }
    
    void generatePrint(const PrintStmtAST& stmt) {
        llvm::Function* printfFunc = module->getFunction("printf");
        
        for (const auto& expr : stmt.expressions) {
            llvm::Value* value = generateExpr(*expr);
            
            if (!value) continue;
            
            if (value->getType()->isIntegerTy(64)) {
                // Print integer
                llvm::Value* formatStr = builder->CreateGlobalString("%ld");
                builder->CreateCall(printfFunc, {formatStr, value});
            } else if (value->getType()->isDoubleTy()) {
                // Print double
                llvm::Value* formatStr = builder->CreateGlobalString("%f");
                builder->CreateCall(printfFunc, {formatStr, value});
            } else if (value->getType()->isPointerTy()) {
                // Print string
                llvm::Value* formatStr = builder->CreateGlobalString("%s");
                builder->CreateCall(printfFunc, {formatStr, value});
            }
        }

        // No automatic newline or flush; printing is exact
    }
    
    void generateInput(const InputStmtAST& stmt) {
        llvm::Function* scanfFunc = module->getFunction("scanf");
        llvm::AllocaInst* variable = namedValues[stmt.varName];
        llvm::GlobalVariable* globalVar = nullptr;
        llvm::Type* varType = nullptr;
        
        // Check local variables first
        if (variable) {
            varType = variable->getAllocatedType();
        } 
        // In session mode, check for global variables
        else if (sessionMode) {
            auto it = replGlobalTypeNames.find(stmt.varName);
            if (it != replGlobalTypeNames.end()) {
                varType = getTypeFromString(it->second);
                globalVar = module->getGlobalVariable(stmt.varName);
                if (!globalVar) {
                    // Create external declaration (no initializer)
                    globalVar = new llvm::GlobalVariable(*module, varType, false,
                                                        llvm::GlobalValue::ExternalLinkage,
                                                        nullptr,  // External declaration
                                                        stmt.varName);
                }
            }
        }
        
        if (!variable && !globalVar) {
            std::cerr << "Undefined variable: " << stmt.varName << std::endl;
            return;
        }
        
        // Use the appropriate variable pointer
        llvm::Value* varPtr = variable ? (llvm::Value*)variable : (llvm::Value*)globalVar;
        
        if (varType->isIntegerTy(64)) {
            // Read as UTF-8 string, then parse to integer via runtime helper
            llvm::Type* i8Ty = llvm::Type::getInt8Ty(*context);
            llvm::AllocaInst* buf = builder->CreateAlloca(i8Ty, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 256), "inbuf");
            llvm::Value* formatStr = builder->CreateGlobalString("%255s");
            builder->CreateCall(scanfFunc, {formatStr, buf});
            // Declare helper: extern long wuyu_parse_int_utf8(const char*)
            llvm::Function* parseFunc = module->getFunction("wuyu_parse_int_utf8");
            if (!parseFunc) {
                llvm::FunctionType* parseTy = llvm::FunctionType::get(
                    llvm::Type::getInt64Ty(*context),
                    { llvm::PointerType::getUnqual(*context) },
                    false);
                parseFunc = llvm::Function::Create(parseTy, llvm::Function::ExternalLinkage,
                                                   "wuyu_parse_int_utf8", module.get());
            }
            llvm::Value* parsed = builder->CreateCall(parseFunc, {buf}, "parsed");
            builder->CreateStore(parsed, varPtr);
        } else if (varType->isDoubleTy()) {
            // Read as UTF-8 string, then parse to double via runtime helper
            llvm::Type* i8Ty = llvm::Type::getInt8Ty(*context);
            llvm::AllocaInst* buf = builder->CreateAlloca(i8Ty, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 256), "inbuf");
            llvm::Value* formatStr = builder->CreateGlobalString("%255s");
            builder->CreateCall(scanfFunc, {formatStr, buf});
            llvm::Function* parseFuncF = module->getFunction("wuyu_parse_float_utf8");
            if (!parseFuncF) {
                llvm::FunctionType* parseTyF = llvm::FunctionType::get(
                    llvm::Type::getDoubleTy(*context),
                    { llvm::PointerType::getUnqual(*context) },
                    false);
                parseFuncF = llvm::Function::Create(parseTyF, llvm::Function::ExternalLinkage,
                                                   "wuyu_parse_float_utf8", module.get());
            }
            llvm::Value* parsedF = builder->CreateCall(parseFuncF, {buf}, "parsedf");
            builder->CreateStore(parsedF, varPtr);
        } else if (varType->isPointerTy()) {
            // Read into a temporary buffer for strings (simplified): allocate 256 bytes
            llvm::Type* i8Ty = llvm::Type::getInt8Ty(*context);
            llvm::AllocaInst* buf = builder->CreateAlloca(i8Ty, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 256), "inbuf");
            llvm::Value* formatStr = builder->CreateGlobalString("%255s");
            builder->CreateCall(scanfFunc, {formatStr, buf});
            // Store pointer into variable
            builder->CreateStore(buf, varPtr);
        }
    }
    
    void generateIf(const IfStmtAST& stmt) {
        llvm::Value* condValue = generateExpr(*stmt.condition);
        
        // Convert condition to bool
        if (condValue->getType()->isIntegerTy() && !condValue->getType()->isIntegerTy(1)) {
            condValue = builder->CreateICmpNE(condValue,
                llvm::ConstantInt::get(condValue->getType(), 0), "ifcond");
        }
        
        llvm::Function* function = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*context, "then", function);
        llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*context, "else", function);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*context, "ifcont", function);
        
        builder->CreateCondBr(condValue, thenBB, elseBB);
        
        // Generate then block
        builder->SetInsertPoint(thenBB);
        for (const auto& s : stmt.thenBranch) {
            generateStmt(*s);
        }
        if (!builder->GetInsertBlock()->getTerminator()) {
            builder->CreateBr(mergeBB);
        }
        
        // Generate else block
        builder->SetInsertPoint(elseBB);
        for (const auto& s : stmt.elseBranch) {
            generateStmt(*s);
        }
        if (!builder->GetInsertBlock()->getTerminator()) {
            builder->CreateBr(mergeBB);
        }
        
        // Merge block
        builder->SetInsertPoint(mergeBB);
    }
    
    void generateWhile(const WhileStmtAST& stmt) {
        llvm::Function* function = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*context, "loop", function);
        llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(*context, "afterloop", function);
        
        // Jump to loop
        builder->CreateBr(loopBB);
        builder->SetInsertPoint(loopBB);
        
        // Generate loop body
        for (const auto& s : stmt.body) {
            generateStmt(*s);
        }
        
        // Check condition
        llvm::Value* condValue = generateExpr(*stmt.condition);
        if (condValue->getType()->isIntegerTy() && !condValue->getType()->isIntegerTy(1)) {
            condValue = builder->CreateICmpNE(condValue,
                llvm::ConstantInt::get(condValue->getType(), 0), "loopcond");
        }
        
        builder->CreateCondBr(condValue, loopBB, afterBB);
        
        // After loop
        builder->SetInsertPoint(afterBB);
    }
    
    void generateFor(const ForStmtAST& stmt) {
        // Allocate loop variable (default int64 for now)
        llvm::Type* type = llvm::Type::getInt64Ty(*context);
        llvm::AllocaInst* loopVar = createEntryBlockAlloca(currentFunction, stmt.varName, type);
        namedValues[stmt.varName] = loopVar;
        
        // Initialize loop variable
        llvm::Value* startVal = generateExpr(*stmt.start);
        builder->CreateStore(startVal, loopVar);
        
        llvm::Function* function = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock* checkBB = llvm::BasicBlock::Create(*context, "forcheck", function);
        llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*context, "forloop", function);
        llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(*context, "afterfor", function);

        // Jump to initial check
        builder->CreateBr(checkBB);

        // Condition check block
        builder->SetInsertPoint(checkBB);
        llvm::Value* condVal = nullptr;
        if (!stmt.isUsingForm) {
            // Compare current to end using '<='
            llvm::Value* currentVal0 = builder->CreateLoad(type, loopVar, stmt.varName);
            llvm::Value* endVal = generateExpr(*stmt.end);
            condVal = builder->CreateICmpSLE(currentVal0, endVal, "forcond");
        } else {
            // Evaluate custom condition expression
            condVal = generateExpr(*stmt.condition);
            if (condVal->getType()->isIntegerTy() && !condVal->getType()->isIntegerTy(1)) {
                condVal = builder->CreateICmpNE(condVal,
                    llvm::ConstantInt::get(condVal->getType(), 0), "forcond");
            }
        }
        builder->CreateCondBr(condVal, loopBB, afterBB);

        // Loop body
        builder->SetInsertPoint(loopBB);
        for (const auto& s : stmt.body) {
            generateStmt(*s);
        }

        // Update step
        if (!stmt.isUsingForm) {
            llvm::Value* currentVal = builder->CreateLoad(type, loopVar, stmt.varName);
            llvm::Value* stepVal = generateExpr(*stmt.step);
            llvm::Value* nextVal = builder->CreateAdd(currentVal, stepVal, "nextvar");
            builder->CreateStore(nextVal, loopVar);
        } else {
            // Compute update expression and store into loop var
            llvm::Value* nextVal = generateExpr(*stmt.update);
            builder->CreateStore(nextVal, loopVar);
        }

        // Re-check condition
        builder->CreateBr(checkBB);

        // After loop
        builder->SetInsertPoint(afterBB);
    }
    
    void generateFunction(const FunctionDeclAST& stmt) {
        // Create function type
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramTypeNames;
        for (const auto& param : stmt.params) {
            paramTypes.push_back(getTypeFromString(param.first));
            paramTypeNames.push_back(param.first);
        }
        
        llvm::Type* returnType = stmt.returnType.empty() ? 
            llvm::Type::getInt64Ty(*context) : getTypeFromString(stmt.returnType);
        std::string returnTypeName = stmt.returnType.empty() ? Keywords::INT : stmt.returnType;
        
        // Track function signature for REPL
        if (sessionMode) {
            replFunctionSignatures[stmt.name] = {paramTypeNames, returnTypeName};
        }
        
        llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
        llvm::Function* function = llvm::Function::Create(funcType,
            llvm::Function::ExternalLinkage, stmt.name, module.get());
        
        // Set parameter names
        size_t idx = 0;
        for (auto& arg : function->args()) {
            arg.setName(stmt.params[idx].second);
            idx++;
        }
        
        // Save current state
        llvm::Function* prevFunction = currentFunction;
        llvm::BasicBlock* prevInsertBlock = builder->GetInsertBlock();
        
        // Create entry block
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", function);
        builder->SetInsertPoint(entry);
        
        currentFunction = function;
        
        // Create allocas for parameters
        std::map<std::string, llvm::AllocaInst*> prevNamedValues = namedValues;
        namedValues.clear();
        
        for (auto& arg : function->args()) {
            llvm::AllocaInst* alloca = createEntryBlockAlloca(function,
                std::string(arg.getName()), arg.getType());
            builder->CreateStore(&arg, alloca);
            namedValues[std::string(arg.getName())] = alloca;
        }
        
        // Generate function body
        for (const auto& s : stmt.body) {
            generateStmt(*s);
        }
        
        // Add return if missing
        if (!builder->GetInsertBlock()->getTerminator()) {
            if (returnType->isVoidTy()) {
                builder->CreateRetVoid();
            } else {
                // For non-void functions, return 0 if no explicit return
                builder->CreateRet(llvm::ConstantInt::get(returnType, 0));
            }
        }
        
        // Restore previous state
        currentFunction = prevFunction;
        if (prevInsertBlock) {
            builder->SetInsertPoint(prevInsertBlock);
        }
        namedValues = prevNamedValues;
    }
    
    void generateReturn(const ReturnStmtAST& stmt) {
        if (stmt.value) {
            llvm::Value* retVal = generateExpr(*stmt.value);
            builder->CreateRet(retVal);
        } else {
            builder->CreateRetVoid();
        }
    }
    
    llvm::Value* generateExpr(const ExprAST& expr) {
        if (auto* intExpr = dynamic_cast<const IntegerExprAST*>(&expr)) {
            return llvm::ConstantInt::get(*context, llvm::APInt(64, intExpr->value, true));
        } else if (auto* floatExpr = dynamic_cast<const FloatExprAST*>(&expr)) {
            return llvm::ConstantFP::get(*context, llvm::APFloat(floatExpr->value));
        } else if (auto* stringExpr = dynamic_cast<const StringExprAST*>(&expr)) {
            return builder->CreateGlobalString(stringExpr->value);
        } else if (auto* boolExpr = dynamic_cast<const BoolExprAST*>(&expr)) {
            return llvm::ConstantInt::get(*context, llvm::APInt(1, boolExpr->value));
        } else         if (auto* varExpr = dynamic_cast<const VariableExprAST*>(&expr)) {
            llvm::AllocaInst* variable = namedValues[varExpr->name];
            if (variable) {
                return builder->CreateLoad(variable->getAllocatedType(), variable, varExpr->name);
            }
            if (sessionMode) {
                auto it = replGlobalTypeNames.find(varExpr->name);
                if (it != replGlobalTypeNames.end()) {
                    llvm::Type* gvType = getTypeFromString(it->second);
                    llvm::GlobalVariable* gv = module->getGlobalVariable(varExpr->name);
                    if (!gv) {
                        // Create external declaration (no initializer)
                        gv = new llvm::GlobalVariable(*module, gvType, false,
                                                      llvm::GlobalValue::ExternalLinkage,
                                                      nullptr,  // External declaration
                                                      varExpr->name);
                    }
                    return builder->CreateLoad(gvType, gv, varExpr->name);
                }
            }
            std::cerr << "Undefined variable: " << varExpr->name << std::endl;
            return nullptr;
        } else if (auto* indexExpr = dynamic_cast<const IndexExprAST*>(&expr)) {
            return generateIndexExpr(*indexExpr);
        } else if (auto* memberExpr = dynamic_cast<const MemberAccessExprAST*>(&expr)) {
            return generateMemberAccess(*memberExpr);
        } else if (auto* binExpr = dynamic_cast<const BinaryExprAST*>(&expr)) {
            return generateBinaryExpr(*binExpr);
        } else if (auto* callExpr = dynamic_cast<const CallExprAST*>(&expr)) {
            return generateCallExpr(*callExpr);
        }
        
        return nullptr;
    }
    
    llvm::Value* generateIndexExpr(const IndexExprAST& expr) {
        // Get array pointer
        llvm::AllocaInst* arrayPtr = namedValues[expr.arrayName];
        if (!arrayPtr) {
            std::cerr << "Undefined array: " << expr.arrayName << std::endl;
            return nullptr;
        }
        
        // For now, support only 1D arrays
        if (expr.indices.size() != 1) {
            std::cerr << "Multi-dimensional array access not yet supported\n";
            return nullptr;
        }
        
        // Generate index
        llvm::Value* index = generateExpr(*expr.indices[0]);
        if (!index) return nullptr;
        
        // Get element pointer
        llvm::Value* elemPtr = builder->CreateGEP(
            arrayPtr->getAllocatedType(), arrayPtr, index, "arrayidx");
        
        // Load the value
        return builder->CreateLoad(arrayPtr->getAllocatedType(), elemPtr, "arrayelem");
    }
    
    llvm::Value* generateMemberAccess(const MemberAccessExprAST& expr) {
        // Get object
        auto* varExpr = dynamic_cast<VariableExprAST*>(expr.object.get());
        if (!varExpr) {
            std::cerr << "Complex member access not yet supported\n";
            return nullptr;
        }
        
        llvm::AllocaInst* objPtr = namedValues[varExpr->name];
        if (!objPtr) {
            std::cerr << "Undefined object: " << varExpr->name << std::endl;
            return nullptr;
        }
        
        // TODO: Implement proper struct member access
        std::cerr << "Member access not yet fully implemented\n";
        return nullptr;
    }
    
    llvm::Value* generateBinaryExpr(const BinaryExprAST& expr) {
        llvm::Value* lhs = generateExpr(*expr.lhs);
        llvm::Value* rhs = generateExpr(*expr.rhs);
        
        if (!lhs || !rhs) return nullptr;
        
        // Handle integer operations
        if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
            if (expr.op == Keywords::ADD || expr.op == "+") {
                return builder->CreateAdd(lhs, rhs, "addtmp");
            } else if (expr.op == Keywords::SUB || expr.op == "-") {
                return builder->CreateSub(lhs, rhs, "subtmp");
            } else if (expr.op == Keywords::MUL || expr.op == "*") {
                return builder->CreateMul(lhs, rhs, "multmp");
            } else if (expr.op == Keywords::DIV || expr.op == "/") {
                return builder->CreateSDiv(lhs, rhs, "divtmp");
            } else if (expr.op == Keywords::FLOORDIV || expr.op == "//") {
                return builder->CreateSDiv(lhs, rhs, "floordivtmp");
            } else if (expr.op == Keywords::MOD || expr.op == "%") {
                return builder->CreateSRem(lhs, rhs, "modtmp");
            } else if (expr.op == Keywords::EQ || expr.op == "==") {
                return builder->CreateICmpEQ(lhs, rhs, "eqtmp");
            } else if (expr.op == Keywords::NEQ || expr.op == "!=") {
                return builder->CreateICmpNE(lhs, rhs, "netmp");
            } else if (expr.op == Keywords::GT || expr.op == ">") {
                return builder->CreateICmpSGT(lhs, rhs, "gttmp");
            } else if (expr.op == Keywords::LT || expr.op == "<") {
                return builder->CreateICmpSLT(lhs, rhs, "lttmp");
            } else if (expr.op == Keywords::GTE || expr.op == ">=") {
                return builder->CreateICmpSGE(lhs, rhs, "getmp");
            } else if (expr.op == Keywords::LTE || expr.op == "<=") {
                return builder->CreateICmpSLE(lhs, rhs, "letmp");
            }
        }
        // Handle floating point operations
        else if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy()) {
            // Promote to double if needed
            if (lhs->getType()->isIntegerTy()) {
                lhs = builder->CreateSIToFP(lhs, llvm::Type::getDoubleTy(*context));
            }
            if (rhs->getType()->isIntegerTy()) {
                rhs = builder->CreateSIToFP(rhs, llvm::Type::getDoubleTy(*context));
            }
            
            if (expr.op == Keywords::ADD || expr.op == "+") {
                return builder->CreateFAdd(lhs, rhs, "faddtmp");
            } else if (expr.op == Keywords::SUB || expr.op == "-") {
                return builder->CreateFSub(lhs, rhs, "fsubtmp");
            } else if (expr.op == Keywords::MUL || expr.op == "*") {
                return builder->CreateFMul(lhs, rhs, "fmultmp");
            } else if (expr.op == Keywords::DIV || expr.op == "/") {
                return builder->CreateFDiv(lhs, rhs, "fdivtmp");
            } else if (expr.op == Keywords::EQ || expr.op == "==") {
                return builder->CreateFCmpOEQ(lhs, rhs, "feqtmp");
            } else if (expr.op == Keywords::NEQ || expr.op == "!=") {
                return builder->CreateFCmpONE(lhs, rhs, "fnetmp");
            } else if (expr.op == Keywords::GT || expr.op == ">") {
                return builder->CreateFCmpOGT(lhs, rhs, "fgttmp");
            } else if (expr.op == Keywords::LT || expr.op == "<") {
                return builder->CreateFCmpOLT(lhs, rhs, "flttmp");
            } else if (expr.op == Keywords::GTE || expr.op == ">=") {
                return builder->CreateFCmpOGE(lhs, rhs, "fgetmp");
            } else if (expr.op == Keywords::LTE || expr.op == "<=") {
                return builder->CreateFCmpOLE(lhs, rhs, "fletmp");
            }
        }
        
        return nullptr;
    }
    
    llvm::Value* generateCallExpr(const CallExprAST& expr) {
        llvm::Function* calleeFunc = module->getFunction(expr.callee);
        
        // If function not found in current module, check if it's a known REPL function
        if (!calleeFunc && sessionMode) {
            auto it = replFunctionSignatures.find(expr.callee);
            if (it != replFunctionSignatures.end()) {
                // Forward declare the function from a previous module
                std::vector<llvm::Type*> paramTypes;
                for (const auto& paramTypeName : it->second.first) {
                    paramTypes.push_back(getTypeFromString(paramTypeName));
                }
                llvm::Type* returnType = getTypeFromString(it->second.second);
                llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
                calleeFunc = llvm::Function::Create(funcType,
                    llvm::Function::ExternalLinkage, expr.callee, module.get());
            }
        }
        
        if (!calleeFunc) {
            std::cerr << "Unknown function: " << expr.callee << std::endl;
            return nullptr;
        }
        
        if (calleeFunc->arg_size() != expr.args.size()) {
            std::cerr << "Incorrect number of arguments for function " << expr.callee 
                      << ": expected " << calleeFunc->arg_size() 
                      << ", got " << expr.args.size() << std::endl;
            return nullptr;
        }
        
        std::vector<llvm::Value*> argsV;
        for (const auto& arg : expr.args) {
            llvm::Value* argVal = generateExpr(*arg);
            if (!argVal) {
                std::cerr << "Failed to generate argument for function " << expr.callee << std::endl;
                return nullptr;
            }
            argsV.push_back(argVal);
        }
        
        return builder->CreateCall(calleeFunc, argsV, "calltmp");
    }
    
    void generateArrayDecl(const ArrayDeclStmtAST& stmt) {
        // Get element type
        llvm::Type* elemType = getTypeFromString(stmt.type);
        
        // For now, support only 1D arrays
        if (stmt.dimensions.size() != 1) {
            std::cerr << "Multi-dimensional arrays not yet fully supported\n";
            return;
        }
        
        // Evaluate dimension
        llvm::Value* sizeVal = generateExpr(*stmt.dimensions[0]);
        if (!sizeVal || !sizeVal->getType()->isIntegerTy()) {
            std::cerr << "Array size must be an integer\n";
            return;
        }
        
        // Create array type (unused for now, but will be needed for proper array implementation)
        // llvm::ArrayType* arrayType = llvm::ArrayType::get(elemType, 0);
        
        // Allocate array on stack (for small arrays) or heap
        llvm::AllocaInst* arrayAlloca = builder->CreateAlloca(elemType, sizeVal, stmt.name);
        namedValues[stmt.name] = arrayAlloca;
        
        // Initialize if provided
        if (stmt.initializer) {
            // Generate the initializer value
            llvm::Value* initValue = generateExpr(*stmt.initializer);
            if (!initValue) {
                std::cerr << "Failed to generate initializer value\n";
                return;
            }
            
            // Create a loop to initialize all elements
            llvm::Function* function = builder->GetInsertBlock()->getParent();
            llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*context, "arrayinit", function);
            llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(*context, "afterinit", function);
            
            // Create index variable
            llvm::AllocaInst* indexAlloca = builder->CreateAlloca(llvm::Type::getInt64Ty(*context), nullptr, "initidx");
            builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0), indexAlloca);
            
            builder->CreateBr(loopBB);
            builder->SetInsertPoint(loopBB);
            
            // Load index
            llvm::Value* indexVal = builder->CreateLoad(llvm::Type::getInt64Ty(*context), indexAlloca, "idx");
            
            // Check if index < size
            llvm::Value* cond = builder->CreateICmpSLT(indexVal, sizeVal, "initcond");
            llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(*context, "initbody", function);
            builder->CreateCondBr(cond, bodyBB, afterBB);
            
            // Body: store value
            builder->SetInsertPoint(bodyBB);
            llvm::Value* elemPtr = builder->CreateGEP(elemType, arrayAlloca, indexVal, "elemptr");
            builder->CreateStore(initValue, elemPtr);
            
            // Increment index
            llvm::Value* nextIndex = builder->CreateAdd(indexVal, 
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 1), "nextidx");
            builder->CreateStore(nextIndex, indexAlloca);
            builder->CreateBr(loopBB);
            
            // Continue after loop
            builder->SetInsertPoint(afterBB);
        }
    }
    
    void generateClass(const ClassDeclAST& stmt) {
        // Create struct type for class
        std::vector<llvm::Type*> memberTypes;
        std::map<std::string, unsigned> memberIndices;
        
        // If there's inheritance, copy base class members
        if (!stmt.baseClass.empty()) {
            if (classTypes.count(stmt.baseClass)) {
                classInheritance[stmt.name] = stmt.baseClass;
                llvm::StructType* baseType = classTypes[stmt.baseClass];
                for (unsigned i = 0; i < baseType->getNumElements(); i++) {
                    memberTypes.push_back(baseType->getElementType(i));
                }
            }
        }
        
        // Create struct type
        llvm::StructType* classType = llvm::StructType::create(*context, stmt.name);
        classTypes[stmt.name] = classType;
        
        // Process member declarations
        std::string prevClass = currentClass;
        currentClass = stmt.name;
        
        for (const auto& member : stmt.members) {
            if (auto* funcDecl = dynamic_cast<FunctionDeclAST*>(member.get())) {
                // Generate method
                generateClassMethod(stmt.name, *funcDecl);
            } else if (auto* varDecl = dynamic_cast<VarDeclStmtAST*>(member.get())) {
                // Add field to struct
                llvm::Type* fieldType = getTypeFromString(varDecl->type);
                memberIndices[varDecl->name] = memberTypes.size();
                memberTypes.push_back(fieldType);
            }
            // TODO: Handle operator overloads
        }
        
        currentClass = prevClass;
        
        // Set struct body
        if (!memberTypes.empty()) {
            classType->setBody(memberTypes);
        }
    }
    
    void generateClassMethod(const std::string& className, const FunctionDeclAST& method) {
        // Create method name: ClassName_methodName
        std::string methodName = className + "_" + method.name;
        
        // Add implicit 'self' parameter
        std::vector<llvm::Type*> paramTypes;
        paramTypes.push_back(llvm::PointerType::get(classTypes[className], 0));
        
        for (const auto& param : method.params) {
            paramTypes.push_back(getTypeFromString(param.first));
        }
        
        llvm::Type* returnType = method.returnType.empty() ? 
            llvm::Type::getVoidTy(*context) : getTypeFromString(method.returnType);
        
        llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
        llvm::Function* function = llvm::Function::Create(funcType,
            llvm::Function::ExternalLinkage, methodName, module.get());
        
        // Store in class methods map
        classMethods[className + "." + method.name] = function;
        
        // Set parameter names
        auto argIt = function->arg_begin();
        argIt->setName(Keywords::SELF);  // self
        argIt++;
        
        for (const auto& param : method.params) {
            argIt->setName(param.second);
            argIt++;
        }
        
        // Save current state
        llvm::Function* prevFunction = currentFunction;
        llvm::BasicBlock* prevInsertBlock = builder->GetInsertBlock();
        
        // Create entry block
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", function);
        builder->SetInsertPoint(entry);
        
        currentFunction = function;
        
        // Create allocas for parameters
        std::map<std::string, llvm::AllocaInst*> prevNamedValues = namedValues;
        namedValues.clear();
        
        for (auto& arg : function->args()) {
            llvm::AllocaInst* alloca = createEntryBlockAlloca(function,
                std::string(arg.getName()), arg.getType());
            builder->CreateStore(&arg, alloca);
            namedValues[std::string(arg.getName())] = alloca;
        }
        
        // Generate method body
        for (const auto& s : method.body) {
            generateStmt(*s);
        }
        
        // Add return if missing
        if (!builder->GetInsertBlock()->getTerminator()) {
            if (returnType->isVoidTy()) {
                builder->CreateRetVoid();
            } else {
                builder->CreateRet(llvm::Constant::getNullValue(returnType));
            }
        }
        
        // Restore previous state
        currentFunction = prevFunction;
        if (prevInsertBlock) {
            builder->SetInsertPoint(prevInsertBlock);
        }
        namedValues = prevNamedValues;
    }
    
    void generateImport(const ImportStmtAST& stmt) {
        // For now, just log the import - full module system requires more infrastructure
        std::cerr << "Import: " << stmt.modulePath << " (module system not yet implemented)\n";
        
        // TODO: Implement full module system:
        // 1. Find module file in search paths (典/ directory, etc.)
        // 2. Parse the module
        // 3. Generate code for it
        // 4. Link the modules
    }
};

} // namespace wuyu


