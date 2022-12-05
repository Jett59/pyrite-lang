#include "escapes.h"
#include <stdexcept>

namespace pyrite {
std::string translateEscapes(const std::string &str) {
  std::string result;
  for (size_t i = 0; i < str.size(); i++) {
    char c = str[i];
    if (c == '\\') {
      if (i + 1 >= str.size()) {
        throw std::runtime_error("Invalid escape sequence");
      }
      char next = str[++i];
      switch (next) {
      case 'a':
        result += '\a';
        break;
      case 'b':
        result += '\b';
        break;
      case 'f':
        result += '\f';
        break;
      case 'n':
        result += '\n';
        break;
      case 'r':
        result += '\r';
        break;
      case 't':
        result += '\t';
        break;
      case 'v':
        result += '\v';
        break;
      case '\\':
        result += '\\';
        break;
      case '\'':
        result += '\'';
        break;
      case '\"':
        result += '\"';
        break;
      default:
        throw std::runtime_error("Invalid escape sequence");
      }
    } else {
      result += c;
    }
  }
  return result;
}
} // namespace pyrite
