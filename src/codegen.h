#ifndef PYRITE_CODEGEN_H
#define PYRITE_CODEGEN_H

#include "ast.h"

namespace pyrite {
enum class CodeFileType { ASSEMBLY, OBJECT, LLVM_ASSEMBLY, LLVM_BITCODE };
enum class OptimizationLevel { NONE, O1, O2, O3 };

void codegen(const AstNode &ast, const std::string &targetTriple,
             const std::string &outputFile, CodeFileType outputFileType,
             OptimizationLevel optimizationLevel);
} // namespace pyrite

#endif