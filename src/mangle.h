#ifndef PYRITE_MANGLE_H
#define PYRITE_MANGLE_H

#include "ast.h"
#include <string>

namespace pyrite {
/**
 * @brief mangle the given function's name
 *
 * @param function the function to mangle the name of
 * @return std::string the mangled name
 *
 * @note C-exported functions are not mangled; their names are returned as-is.
 */
std::string mangle(const FunctionDefinitionNode &function);
std::string mangle(const std::string &name, const FunctionType &type);
} // namespace pyrite

#endif