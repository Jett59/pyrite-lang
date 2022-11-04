#ifndef PYRITE_ERROR_H
#define PYRITE_ERROR_H

#include <string>

namespace pyrite {
class PyriteError {
public:
  PyriteError(std::string message) : message(std::move(message)) {}

  const std::string &getMessage() const { return message; }

private:
  std::string message;
};
} // namespace pyrite

#endif