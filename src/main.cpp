#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "jit.h"
#include "compiler.h"
#include "keywords.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstring>

#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

using namespace wuyu;

// ANSI color codes
#define COLOR_RED     "\033[91m"
#define COLOR_GREEN   "\033[92m"
#define COLOR_YELLOW  "\033[93m"
#define COLOR_RESET   "\033[0m"

void printUsage(const char* programName) {
    std::cout << "無語編譯器《零點零點一》\n";
    std::cout << "版權所有 宋亦丁 二零二五 乙巳年\n\n";
    std::cout << "無語之法:\n";
    std::cout << "  " << programName << " [則令] <文檔之名>\n\n";
    std::cout << "無語則令:\n";
    std::cout << "  -i, --interactive    REPL 之\n";
    std::cout << "  -c, --compile        譯出標檔 (.o)\n";
    std::cout << "  -l, --llvm-ir        譯出 LLVM 中階之式 (.ll)\n";
    std::cout << "  -o, --output <file>  出檔何呼\n";
    std::cout << "  -h, --help           解此令也\n";
    std::cout << "  -v, --version        無語之版\n\n";
    std::cout << "例:\n";
    std::cout << "  " << programName << " program.🐲                    # 急譯之\n";
    std::cout << "  " << programName << " -c program.wuyu -o out.o      # 譯出標檔\n";
    std::cout << "  " << programName << " -l program.無 -o out.ll       # 譯出 LLVM 中階之式\n";
    std::cout << "  " << programName << " -i                            # 時習之\n";
}

void printVersion() {
    std::cout << "無語編譯器《零點零點一》\n";
    std::cout << "版權所有 宋亦丁 二零二五 乙巳年\n\n";
    std::cout << "基於 LLVM " << LLVM_VERSION_MAJOR << "." 
              << LLVM_VERSION_MINOR << "." << LLVM_VERSION_PATCH << "\n";
}

std::string readFile(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        std::cerr << "錯：「" << path << "」開之不得也。\n";
        return "";
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

bool executeCode(const std::string& source, bool jit = true, 
                 const std::string& outputPath = "") {
    try {
        // Lexical analysis
        wuyu::Lexer lexer(source);
        auto tokens = lexer.tokenize();
        
        if (tokens.empty()) {
            std::cerr << "錯：無代碼生成也。\n";
            return false;
        }
        
        // Parsing
        wuyu::Parser parser(tokens);
        auto ast = parser.parse();
        
        if (!ast) {
            std::cerr << "錯：解析程序敗也。\n";
            return false;
        }
        
        // Code generation
        wuyu::CodeGenerator codegen("wuyu_module");
        codegen.generate(*ast);
        
        if (jit) {
            // JIT execution
            wuyu::JITEngine jitEngine;
            
            // Get module before releasing
            auto modulePtr = codegen.releaseModule();
            auto& context = codegen.getContext();
            
            if (!jitEngine.addModule(std::move(modulePtr), context)) {
                std::cerr << "錯：添入 JIT 敗也。\n";
                return false;
            }
            
            (void)jitEngine.runMain();
        } else {
            // AOT compilation
            auto& module = codegen.getModule();
            
            if (outputPath.empty()) {
                // Print IR to stdout
                codegen.printIR();
            } else {
                // Determine output type from extension
                std::string ext = outputPath.substr(outputPath.find_last_of(".") + 1);
                
                if (ext == "ll") {
                    wuyu::Compiler::compileToLLVMIR(module, outputPath);
                } else {
                    wuyu::Compiler::compileToObjectFile(module, outputPath);
                }
            }
        }
        
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "錯：" << e.what() << "\n";
        return false;
    }
}

// REPL State - maintains context across multiple statements
class REPLContext {
private:
    std::unique_ptr<wuyu::JITEngine> jit;
    std::map<std::string, std::string> globalTypeMap;  // Track variable types across sessions
    std::map<std::string, std::pair<std::vector<std::string>, std::string>> functionSignatures;  // Track functions
    int statementCount;
    
public:
    REPLContext() : jit(std::make_unique<wuyu::JITEngine>()), statementCount(0) {}
    
    bool executeStatement(const std::string& source) {
        try {
            // Parse just this new statement
            wuyu::Lexer lexer(source);
            std::vector<wuyu::Token> tokens = lexer.tokenize();
            if (tokens.empty()) return true;

            wuyu::Parser parser(tokens);
            auto program = parser.parse();
            if (!program || program->statements.empty()) {
                std::cerr << COLOR_RED << "錯：解析有誤。" << COLOR_RESET << "\n";
                return false;
            }

            // Generate code with session mode enabled for REPL
            std::string entryName = "__wuyu_repl_stmt_" + std::to_string(statementCount++);
            std::string moduleName = "wuyu_repl_" + std::to_string(statementCount);
            wuyu::CodeGenerator codegen(moduleName);
            
            // Enable session mode and import existing global variables and functions
            codegen.setSessionMode(true);
            codegen.setCustomEntryName(entryName);
            codegen.importReplTypeMap(globalTypeMap);
            codegen.importReplFunctionSignatures(functionSignatures);
            
            // Generate the code
            codegen.generate(*program);
            
            // Export updated type map and function signatures for next iteration
            globalTypeMap = codegen.exportReplTypeMap();
            functionSignatures = codegen.exportReplFunctionSignatures();

            // Add module to the persistent JIT
            if (!jit->addModule(codegen.releaseModule(), codegen.getContext())) {
                std::cerr << COLOR_RED << "錯：添入 JIT 敗也。" << COLOR_RESET << "\n";
                return false;
            }
            
            // Execute the entry function (using entryName we already have)
            (void)jit->runFunction(entryName);
            return true;
            
        } catch (const std::exception& e) {
            std::cerr << COLOR_RED << "錯：" << e.what() << COLOR_RESET << "\n";
            return false;
        }
    }
    
    void reset() {
        jit = std::make_unique<wuyu::JITEngine>();
        globalTypeMap.clear();
        functionSignatures.clear();
        statementCount = 0;
    }
};

void runREPL() {
    std::cout << "無語編譯器《零點零點一》\n";
    std::cout << "版權所有 宋亦丁 二零二五 乙巳年\n\n";
    
    REPLContext replContext;
    std::string line;
    std::string buffer;
    bool inBlockDefinition = false;  // Track if we're inside a multi-line block
    bool showPrompt = true;  // Control when to show prompt
    
    while (true) {
        // Get input line
#ifdef HAVE_READLINE
        // Use readline for better editing experience
        const char* prompt = (buffer.empty() ? "令 " : "納 ");
        char* input = nullptr;
        
        if (showPrompt) {
            input = readline(prompt);
        } else {
            input = readline("");
        }
        
        if (!input) {
            // EOF or error
            if (!buffer.empty()) {
                std::cout << "\n";
                replContext.executeStatement(buffer);
                buffer.clear();
            }
            break;
        }
        
        line = input;
        
        // Add to history if non-empty and not just whitespace
        if (line.find_first_not_of(" \t\r\n") != std::string::npos) {
            add_history(input);
        }
        
        free(input);
        showPrompt = true;  // Reset for next iteration
#else
        // Fallback to basic getline
        if (showPrompt) {
            std::cout << (buffer.empty() ? "令 " : "納 ");
            std::cout.flush();
        }
        showPrompt = true;  // Reset for next iteration

        if (!std::getline(std::cin, line)) {
            // EOF or error
            if (!buffer.empty()) {
                std::cout << "\n";
                replContext.executeStatement(buffer);
                buffer.clear();
            }
            break;
        }
#endif

        // Exit commands (only if not in the middle of a block)
        if (!inBlockDefinition && (line == "走" || line == "屁" || line == "exit" || line == "quit")) {
            break;
        }
        
        // Reset command
        if (!inBlockDefinition && (line == "清" || line == "reset")) {
            replContext.reset();
            buffer.clear();
            std::cout << COLOR_GREEN << "告：狀態已清。" << COLOR_RESET << "\n";
            continue;
        }

        // Trim line to check content
        std::string trimmedLine = line;
        size_t start = trimmedLine.find_first_not_of(" \t\r\n");
        if (start != std::string::npos) {
            trimmedLine = trimmedLine.substr(start);
        } else {
            trimmedLine = "";
        }
        
        // Check if this line starts a block definition
        bool isBlockStart = (trimmedLine.find("設術") == 0 || trimmedLine.find("設類") == 0 ||
                             trimmedLine.find(Keywords::WHILE) == 0 || trimmedLine.find(Keywords::FOR) == 0);
        
        // If we're starting a block, enter block mode
        if (isBlockStart) {
            inBlockDefinition = true;
        }
        
        // If user enters empty line when not in a block, either:
        // - Execute the block if we were in block mode, or
        // - Reset to clean 令 prompt if buffer has incomplete content
        if (trimmedLine.empty() && !isBlockStart) {
            if (inBlockDefinition) {
                // Empty line signals end of block - execute it
                buffer += line + "\n";
                inBlockDefinition = false;
                replContext.executeStatement(buffer);
                buffer.clear();
                continue;
            } else if (!buffer.empty()) {
                // User pressed enter on incomplete statement - discard it
                buffer.clear();
                continue;
            } else {
                // Empty line with empty buffer - don't show prompt again
                showPrompt = false;
                continue;
            }
        }
        
        // Append line to buffer
        buffer += line + "\n";
        
        // If in block definition mode, continue accumulating
        if (inBlockDefinition) {
            continue;
        }
        
        // For non-block statements, check for statement terminator (。)
        bool inString = false;
        bool hasStatementEnd = false;
        
        for (size_t i = 0; i < line.size();) {
            unsigned char c = static_cast<unsigned char>(line[i]);
            
            // Detect multibyte sequences for quotes and period
            if (i + 2 < line.size()) {
                unsigned char c1 = c, c2 = static_cast<unsigned char>(line[i+1]), 
                             c3 = static_cast<unsigned char>(line[i+2]);
                             
                // UTF-8 for 「(E3 80 8C), 」(E3 80 8D), 『(E3 80 8E), 』(E3 80 8F), 。(E3 80 82)
                if (c1 == 0xE3 && c2 == 0x80) {
                    if (c3 == 0x8C || c3 == 0x8E) {
                        inString = true;
                        i += 3;
                        continue;
                    } else if (c3 == 0x8D || c3 == 0x8F) {
                        inString = false;
                        i += 3;
                        continue;
                    } else if (c3 == 0x82 && !inString) {
                        hasStatementEnd = true;
                        i += 3;
                        continue;
                    }
                }
            }
            i++;
        }
        
        // Execute if we found a statement terminator
        if (hasStatementEnd) {
            replContext.executeStatement(buffer);
            buffer.clear();
        }
    }
    
    std::cout << "\n浮雲一別後，流水十年間。\n";
}

int main(int argc, char* argv[]) {
    // Parse command line arguments
    bool interactive = false;
    bool compile = false;
    bool emitLLVM = false;
    std::string inputFile;
    std::string outputFile;
    
    // If no arguments provided, default to interactive mode
    if (argc < 2) {
        interactive = true;
    }
    
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        
        if (arg == "-h" || arg == "--help") {
            printUsage(argv[0]);
            return 0;
        } else if (arg == "-v" || arg == "--version") {
            printVersion();
            return 0;
        } else if (arg == "-i" || arg == "--interactive") {
            interactive = true;
        } else if (arg == "-c" || arg == "--compile") {
            compile = true;
        } else if (arg == "-l" || arg == "--llvm-ir") {
            emitLLVM = true;
        } else if (arg == "-o" || arg == "--output") {
            if (i + 1 < argc) {
                outputFile = argv[++i];
            } else {
                std::cerr << Keywords::ERROR_PREFIX << "-o " << Keywords::ERROR_NEEDS_PARAM << "\n";
                return 1;
            }
        } else if (arg[0] != '-') {
            inputFile = arg;
        } else {
            std::cerr << Keywords::ERROR_PREFIX << Keywords::ERROR_UNKNOWN_OPTION << "\n";
            printUsage(argv[0]);
            return 1;
        }
    }
    
    // Interactive mode
    if (interactive) {
        runREPL();
        return 0;
    }
    
    // File mode
    if (inputFile.empty()) {
        std::cerr << "錯：無輸入文檔也。\n";
        printUsage(argv[0]);
        return 1;
    }
    
    std::string source = readFile(inputFile);
    if (source.empty()) {
        return 1;
    }
    
    // Determine mode
    bool jitMode = !compile && !emitLLVM;
    
    if (jitMode) {
        // JIT execution
        return executeCode(source, true) ? 0 : 1;
    } else {
        // AOT compilation
        if (outputFile.empty()) {
            // Default output name
            size_t lastDot = inputFile.find_last_of(".");
            std::string baseName = (lastDot != std::string::npos) ? 
                inputFile.substr(0, lastDot) : inputFile;
            
            if (emitLLVM) {
                outputFile = baseName + ".ll";
            } else {
                outputFile = baseName + ".o";
            }
        }
        
        return executeCode(source, false, outputFile) ? 0 : 1;
    }
}
