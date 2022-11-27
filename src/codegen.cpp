#include "codegen.h"
#include "optimizations.h"
#include <iostream>
#include <llvm/Bitcode/BitcodeWriterPass.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace llvm;

namespace pyrite {
void codegen(const AstNode &ast, const std::string &targetTriple,
             const std::string &outputFile, CodeFileType outputFileType,
             OptimizationLevel optimizationLevel) {
  InitializeAllTargets();
  InitializeAllTargetInfos();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
  LLVMContext context;
  Module module(outputFile, context);
  std::string error;
  std::string defaultTargetTriple = sys::getDefaultTargetTriple();
  const std::string *selectedTargetTriple = &targetTriple;
  if (targetTriple.empty()) {
    selectedTargetTriple = &defaultTargetTriple;
  }
  module.setTargetTriple(*selectedTargetTriple);
  auto target = TargetRegistry::lookupTarget(*selectedTargetTriple, error);
  if (!target) {
    throw std::runtime_error(error);
  }
  auto cpu = "generic";
  auto features = "";
  TargetOptions targetOptions;
  auto relocationModel = Optional<Reloc::Model>();
  auto targetMachine = target->createTargetMachine(
      targetTriple, cpu, features, targetOptions, relocationModel);
  module.setDataLayout(targetMachine->createDataLayout());
  // TODO: codegen
  verifyModule(module, &errs());
  optimize(module, optimizationLevel);
  std::error_code errorCode;
  raw_fd_ostream dest(outputFile, errorCode, sys::fs::OF_None);
  if (errorCode) {
    throw std::runtime_error("Error: " + errorCode.message());
  }
  legacy::PassManager codegenPassManager;
  if (outputFileType == CodeFileType::ASSEMBLY) {
    targetMachine->addPassesToEmitFile(codegenPassManager, dest, nullptr,
                                       CGFT_AssemblyFile);
  } else if (outputFileType == CodeFileType::OBJECT) {
    targetMachine->addPassesToEmitFile(codegenPassManager, dest, nullptr,
                                       CGFT_ObjectFile);
  } else if (outputFileType == CodeFileType::LLVM_ASSEMBLY) {
    codegenPassManager.add(createPrintModulePass(dest));
  } else if (outputFileType == CodeFileType::LLVM_BITCODE) {
    codegenPassManager.add(createBitcodeWriterPass(dest));
  } else {
    throw std::runtime_error("Error: unsupported output file type");
  }
  codegenPassManager.run(module);
}
} // namespace pyrite
