#ifndef PYRITE_OPTIMIZATIONS_H
#define PYRITE_OPTIMIZATIONS_H

#include "codegen.h"
#include <llvm/IR/Module.h>

namespace pyrite {
void optimize(llvm::Module &module, OptimizationLevel optimizationLevel);
}

#endif