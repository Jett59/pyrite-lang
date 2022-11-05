#ifndef PYRITE_LEXER_H
#define PYRITE_LEXER_H

#include "location.hh"
#include "parser.hh"
#include <fstream>
#include <string>

namespace pyrite {
class Lexer {
public:
  Lexer(std::ifstream &file) : file(file) { currentLocation.initialize(); }

  Parser::symbol_type nextToken();

  char nextCharacter();
  void unreadCharacter(char c);

private:
  std::ifstream &file;
  location currentLocation;
  std::string buffer;
  size_t bufferIndex = 0;
};
} // namespace pyrite

#endif