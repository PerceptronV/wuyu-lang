#pragma once

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <memory>
#include <iostream>

namespace wuyu {

// Forward declaration of runtime helper provided by host
extern "C" long wuyu_parse_int_utf8(const char*);
extern "C" double wuyu_parse_float_utf8(const char*);

class JITEngine {
private:
    std::unique_ptr<llvm::orc::LLJIT> jit;
    bool runtimeSymbolsDefined;
    
public:
    JITEngine() : runtimeSymbolsDefined(false) {
        // Initialize native target
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        
        // Create JIT
        auto jitOrErr = llvm::orc::LLJITBuilder().create();
        if (!jitOrErr) {
            llvm::errs() << "Failed to create JIT: " << jitOrErr.takeError() << "\n";
            return;
        }
        jit = std::move(*jitOrErr);
        
        // Expose runtime helper for Chinese digit parsing to JIT (only once)
        auto &jd = jit->getMainJITDylib();
        auto errRt = jd.define(llvm::orc::absoluteSymbols({
            { jit->mangleAndIntern("wuyu_parse_int_utf8"),
              { llvm::orc::ExecutorAddr::fromPtr((void*)&wuyu_parse_int_utf8), llvm::JITSymbolFlags::Exported } },
            { jit->mangleAndIntern("wuyu_parse_float_utf8"),
              { llvm::orc::ExecutorAddr::fromPtr((void*)&wuyu_parse_float_utf8), llvm::JITSymbolFlags::Exported } }
        }));
        if (errRt) {
            llvm::errs() << "Failed to define runtime symbols: " << errRt << "\n";
        } else {
            runtimeSymbolsDefined = true;
        }
    }
    
    bool addModule(std::unique_ptr<llvm::Module> module, llvm::LLVMContext& context) {
        if (!jit) return false;

        // Optimize module
        optimizeModule(*module);
        
        // Add module to JIT
        auto tsm = llvm::orc::ThreadSafeModule(std::move(module),
                                               std::make_unique<llvm::LLVMContext>());
        
        auto err = jit->addIRModule(std::move(tsm));
        if (err) {
            llvm::errs() << "Failed to add module: " << err << "\n";
            return false;
        }
        
        return true;
    }
    
    int runMain() {
        if (!jit) return -1;
        
        // Look up main function
        auto mainSymbol = jit->lookup("main");
        if (!mainSymbol) {
            llvm::errs() << "Failed to find main: " << mainSymbol.takeError() << "\n";
            return -1;
        }
        
        // Cast to function pointer and execute
        auto mainFunc = mainSymbol->toPtr<int(*)()>();
        return mainFunc();
    }

    int runFunction(const std::string& name) {
        if (!jit) return -1;
        auto sym = jit->lookup(name);
        if (!sym) {
            llvm::errs() << "Failed to find function '" << name << "'\n";
            return -1;
        }
        auto fn = sym->toPtr<int(*)()>();
        return fn();
    }
    
private:
    void optimizeModule(llvm::Module& module) {
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
        
        // Apply optimization pipeline (O0 for debugging)
        llvm::ModulePassManager modulePassManager =
            passBuilder.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
        
        modulePassManager.run(module, moduleAnalysisManager);
    }
};

} // namespace wuyu

