#ifndef PYRITE_ERROR_H
#define PYRITE_ERROR_H

#include "ast.h"
#include <iostream>
#include <string>

namespace pyrite {
class PyriteError;
static void warn(const PyriteError &exception);

class PyriteError {
public:
  PyriteError(std::string message, size_t line, size_t column)
      : message(std::move(message)), line(line), column(column) {}
  PyriteError(std::string message, const AstMetadata &metadata)
      : message(std::move(message)), line(metadata.line),
        column(metadata.column) {}

  std::string getMessage() const {
    return std::to_string(line) + ":" + std::to_string(column) + ": " + message;
  }

private:
  std::string message;
  size_t line, column;

  friend void warn(const PyriteError &);
};

// I don't think there is an easier way to allow easy access to the list of
// errors (especially in the parser where I have no control over the arguments
// passed to yyerror) so a global variable will have to do.
extern std::vector<PyriteError> errors;

static inline void warn(const std::string &message, size_t line,
                        size_t column) {
  std::cerr << "warning: " << line << ":" << column << ": " << message
            << std::endl;
}
static inline void warn(const std::string &message,
                        const AstMetadata &metadata) {
  warn(message, metadata.line, metadata.column);
}
static inline void warn(const PyriteError &exception) {
  warn(exception.message, exception.line, exception.column);
}
} // namespace pyrite

#endif