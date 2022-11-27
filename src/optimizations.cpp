#include "codegen.h"
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
  ModuleAnalysisManager moduleAnalyzer;
  PassBuilder passBuilder;
  FunctionAnalysisManager functionAnalyzer;
  CGSCCAnalysisManager cgsccAnalyzer;
  LoopAnalysisManager loopAnalyzer;
  moduleAnalyzer.registerPass(
      [&] { return FunctionAnalysisManagerModuleProxy(functionAnalyzer); });
  passBuilder.registerModuleAnalyses(moduleAnalyzer);
  passBuilder.registerCGSCCAnalyses(cgsccAnalyzer);
  passBuilder.registerFunctionAnalyses(functionAnalyzer);
  passBuilder.registerLoopAnalyses(loopAnalyzer);
  passBuilder.crossRegisterProxies(loopAnalyzer, functionAnalyzer,
                                   cgsccAnalyzer, moduleAnalyzer);
  ModulePassManager passManager =
      passBuilder.buildPerModuleDefaultPipeline(getLevel(optimizationLevel));
  passManager.run(module, moduleAnalyzer);
}
} // namespace pyrite
