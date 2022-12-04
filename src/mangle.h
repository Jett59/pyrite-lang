#ifndef PYRITE_MANGLE_H
#define PYRITE_MANGLE_H

#include "ast.h"
#include <string>

namespace pyrite {
std::string mangle(const FunctionDefinitionNode &function);
std::string mangle(const std::string &name, const FunctionType &type);
} // namespace pyrite

#endif