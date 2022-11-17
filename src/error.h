#ifndef PYRITE_ERROR_H
#define PYRITE_ERROR_H

#include "ast.h"
#include <iostream>
#include <string>

namespace pyrite {
class PyriteException;
static void warn(const PyriteException &exception);

class PyriteException {
public:
  PyriteException(std::string message, size_t line, size_t column)
      : message(std::move(message)), line(line), column(column) {}
  PyriteException(std::string message, const AstMetadata &metadata)
      : message(std::move(message)), line(metadata.line),
        column(metadata.column) {}

  std::string getMessage() const {
    return std::to_string(line) + ":" + std::to_string(column) + ": " + message;
  }

private:
  std::string message;
  size_t line, column;

  friend void warn(const PyriteException &);
};

static inline void warn(const std::string &message, size_t line,
                        size_t column) {
  std::cerr << "warning: " << line << ":" << column << ": " << message
            << std::endl;
}
static inline void warn(const std::string &message,
                        const AstMetadata &metadata) {
  warn(message, metadata.line, metadata.column);
}
static inline void warn(const PyriteException &exception) {
  warn(exception.message, exception.line, exception.column);
}
} // namespace pyrite

#endif