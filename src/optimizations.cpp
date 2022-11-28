#include "codegen.h"
#include <iostream>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Vectorize.h>

using namespace llvm;

namespace pyrite {
static llvm::OptimizationLevel getLevel(pyrite::OptimizationLevel level) {
  switch (level) {
  case pyrite::OptimizationLevel::NONE:
    return llvm::OptimizationLevel::O0;
  case pyrite::OptimizationLevel::O1:
    return llvm::OptimizationLevel::O1;
  case pyrite::OptimizationLevel::O2:
    return llvm::OptimizationLevel::O2;
  case pyrite::OptimizationLevel::O3:
    return llvm::OptimizationLevel::O3;
  }
  return llvm::OptimizationLevel::O0;
}

void optimize(Module &module, OptimizationLevel optimizationLevel) {
  if (optimizationLevel != OptimizationLevel::NONE) {
    LoopAnalysisManager loopAnalysisManager;
    FunctionAnalysisManager functionAnalysisManager;
    CGSCCAnalysisManager cGSCCAnalysisManager;
    ModuleAnalysisManager moduleAnalysisManager;
    PassBuilder passBuilder;
    passBuilder.registerModuleAnalyses(moduleAnalysisManager);
    passBuilder.registerCGSCCAnalyses(cGSCCAnalysisManager);
    passBuilder.registerFunctionAnalyses(functionAnalysisManager);
    passBuilder.registerLoopAnalyses(loopAnalysisManager);
    passBuilder.crossRegisterProxies(
        loopAnalysisManager, functionAnalysisManager, cGSCCAnalysisManager,
        moduleAnalysisManager);
    ModulePassManager passManager =
        passBuilder.buildPerModuleDefaultPipeline(getLevel(optimizationLevel));
    passManager.run(module, moduleAnalysisManager);
  }
}
} // namespace pyrite
