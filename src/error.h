#ifndef PYRITE_ERROR_H
#define PYRITE_ERROR_H

#include <string>

namespace pyrite {
class PyriteException {
public:
  PyriteException(std::string message, size_t line, size_t column)
      : message(std::move(message)), line(line), column(column) {}

  std::string getMessage() const {
    return std::to_string(line) + ":" + std::to_string(column) + ": " + message;
  }

private:
  std::string message;
  size_t line, column;
};
} // namespace pyrite

#endif