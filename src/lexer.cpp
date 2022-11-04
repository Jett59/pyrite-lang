#include "lexer.h"
#include <optional>
#include <utility>

namespace pyrite {
char Lexer::nextCharacter() {
  char c;
  if (bufferIndex < buffer.length()) {
    c = buffer[bufferIndex++];
  } else {
    if (file.eof()) {
      c = '\0';
    } else {
      file.get(c);
      buffer += c;
      bufferIndex++;
    }
  }
  if (c == '\n') {
    currentLocation.end.line++;
    currentLocation.end.column = 0;
  } else {
    currentLocation.end.column++;
  }
  return c;
}
void Lexer::unreadCharacter(char c) {
  if (bufferIndex > 0) {
    bufferIndex--;
  } else {
    buffer = c + buffer;
  }
  if (c == '\n') {
    currentLocation.end.line--;
    currentLocation.end.column = 0;
  } else {
    currentLocation.end.column--;
  }
}

using TokenConstructor = Parser::symbol_type (*)(location location);

// Annoying c++ doesn't allow passing string literals to templates properly.
// This is because std::string has private members, so you can't pass it to a
// template.
// We can pass this helper struct though.
template <size_t size> struct StringLiteral {
  char data[size];
  StringLiteral(const char (&str)[size]) {
    for (size_t i = 0; i < size; i++) {
      data[i] = str[i];
    }
  }
  char operator[](size_t index) const { return data[index]; }
};

using TokenRule = std::optional<Parser::symbol_type> (*)(Lexer &lexer,
                                                         location &location);

template <TokenConstructor tokenConstructor, StringLiteral string>
std::optional<Parser::symbol_type> basicTokenRule(Lexer &lexer,
                                                  location &location) {
  for (char c : string.data) {
    if (lexer.nextCharacter() != c) {
      lexer.unreadCharacter(c);
      return std::nullopt;
    }
  }
  return tokenConstructor(location);
}

std::optional<Parser::symbol_type> identifierTokenRule(Lexer &lexer,
                                                       location &location) {
  char c = lexer.nextCharacter();
  if (!isalpha(c) && c != '_') {
    lexer.unreadCharacter(c);
    return std::nullopt;
  }
  std::string identifier;
  identifier += c;
  while (true) {
    c = lexer.nextCharacter();
    if (!isalnum(c) && c != '_') {
      lexer.unreadCharacter(c);
      break;
    }
    identifier += c;
  }
  return Parser::make_IDENTIFIER(identifier, location);
}

std::optional<Parser::symbol_type> integerLiteralTokenRule(Lexer &lexer,
                                                           location &location) {
  char c = lexer.nextCharacter();
  if (!isdigit(c)) {
    lexer.unreadCharacter(c);
    return std::nullopt;
  }
  std::string integerLiteral;
  integerLiteral += c;
  while (true) {
    c = lexer.nextCharacter();
    if (!isdigit(c)) {
      lexer.unreadCharacter(c);
      break;
    }
    integerLiteral += c;
  }
  return Parser::make_INTEGER_LITERAL(std::stoll(integerLiteral), location);
}

std::optional<Parser::symbol_type> stringLiteralTokenRule(Lexer &lexer,
                                                          location &location) {
  char c = lexer.nextCharacter();
  if (c != '"') {
    lexer.unreadCharacter(c);
    return std::nullopt;
  }
  std::string stringLiteral;
  while (true) {
    c = lexer.nextCharacter();
    if (c == '\\') {
      // Skip the next character as it's escaped and we don't want to terminate
      // the string early.
      stringLiteral += c;
      c = lexer.nextCharacter();
    } else if (c == '"') {
      break;
    }
    stringLiteral += c;
  }
  return Parser::make_STRING_LITERAL(stringLiteral, location);
}

static TokenRule tokenRules[] = {
    identifierTokenRule,
    integerLiteralTokenRule,
    stringLiteralTokenRule,
};

Parser::symbol_type Lexer::nextToken() {
  // Skip all whitespace characters.
  {
    char lastCharacter;
    while (isspace(lastCharacter = nextCharacter())) {
      // Do nothing.
    }
    unreadCharacter(lastCharacter);
  }
  std::vector<std::pair<Parser::symbol_type, location>> possibleTokens;
  // Reverse the end and begin of the location so the end of the last token is
  // the begin of the new one.
  currentLocation.begin = currentLocation.end;
  location originalLocation = currentLocation;
  size_t originalBufferIndex = bufferIndex;
  for (const auto &tokenRule : tokenRules) {
    auto token = tokenRule(*this, currentLocation);
    if (token) {
      possibleTokens.push_back({*token, currentLocation});
    }
    currentLocation = originalLocation;
    bufferIndex = originalBufferIndex;
  }
  if (possibleTokens.empty()) {
    if (file.eof()) {
      return Parser::make_YYEOF(currentLocation);
    } else {
      return Parser::make_YYUNDEF(currentLocation);
    }
  } else {
    // Find the one which takes up the most characters.
    auto bestMatch = &possibleTokens[0];
    auto bestLocation = &bestMatch->second;
    for (auto &match : possibleTokens) {
      auto &matchLocation = match.second;
      if (matchLocation.end.line > bestLocation->end.line ||
          (matchLocation.end.line == bestLocation->end.line &&
           matchLocation.end.column > bestLocation->end.column)) {
        bestMatch = &match;
        bestLocation = &matchLocation;
      }
    }
    return bestMatch->first;
  }
}

} // namespace pyrite
