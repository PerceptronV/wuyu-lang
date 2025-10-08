#pragma once

#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <string>
#include <iostream>

namespace wuyu {

class Compiler {
public:
    static bool compileToObjectFile(llvm::Module& module, const std::string& outputPath) {
        // Initialize native target only
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        
        // Get target triple
        auto targetTriple = llvm::sys::getDefaultTargetTriple();
        module.setTargetTriple(targetTriple);
        
        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
        
        if (!target) {
            llvm::errs() << "Failed to lookup target: " << error << "\n";
            return false;
        }
        
        // Create target machine
        auto cpu = "generic";
        auto features = "";
        llvm::TargetOptions opt;
        auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt,
                                                         llvm::Reloc::PIC_);
        
        module.setDataLayout(targetMachine->createDataLayout());
        
        // Optimize module
        optimizeModule(module);
        
        // Open output file
        std::error_code ec;
        llvm::raw_fd_ostream dest(outputPath, ec, llvm::sys::fs::OF_None);
        
        if (ec) {
            llvm::errs() << "Could not open file: " << ec.message() << "\n";
            return false;
        }
        
        // Emit object file
        llvm::legacy::PassManager pass;
        auto fileType = llvm::CodeGenFileType::ObjectFile;
        
        if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
            llvm::errs() << "Target machine can't emit a file of this type\n";
            return false;
        }
        
        pass.run(module);
        dest.flush();
        
        std::cout << "Object file written to: " << outputPath << std::endl;
        return true;
    }
    
    static bool compileToLLVMIR(llvm::Module& module, const std::string& outputPath) {
        // Optimize module
        optimizeModule(module);
        
        std::error_code ec;
        llvm::raw_fd_ostream dest(outputPath, ec, llvm::sys::fs::OF_None);
        
        if (ec) {
            llvm::errs() << "Could not open file: " << ec.message() << "\n";
            return false;
        }
        
        module.print(dest, nullptr);
        dest.flush();
        
        std::cout << "LLVM IR written to: " << outputPath << std::endl;
        return true;
    }
    
private:
    static void optimizeModule(llvm::Module& module) {
        // Create optimization passes
        llvm::PassBuilder passBuilder;
        
        llvm::LoopAnalysisManager loopAnalysisManager;
        llvm::FunctionAnalysisManager functionAnalysisManager;
        llvm::CGSCCAnalysisManager cgsccAnalysisManager;
        llvm::ModuleAnalysisManager moduleAnalysisManager;
        
        passBuilder.registerModuleAnalyses(moduleAnalysisManager);
        passBuilder.registerCGSCCAnalyses(cgsccAnalysisManager);
        passBuilder.registerFunctionAnalyses(functionAnalysisManager);
        passBuilder.registerLoopAnalyses(loopAnalysisManager);
        passBuilder.crossRegisterProxies(loopAnalysisManager, functionAnalysisManager,
                                        cgsccAnalysisManager, moduleAnalysisManager);
        
        // Apply optimization pipeline (O2)
        llvm::ModulePassManager modulePassManager =
            passBuilder.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
        
        modulePassManager.run(module, moduleAnalysisManager);
    }
};

} // namespace wuyu

