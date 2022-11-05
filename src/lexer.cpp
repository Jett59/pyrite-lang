#include "lexer.h"
#include <functional>
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

using TokenConstructor = std::function<Parser::symbol_type()>;

template <typename Function>
concept TokenConstructorFunction =
    std::is_same_v<std::invoke_result_t<Function>, Parser::symbol_type>;

// Quick sanity check: TokenConstructor is a TokenConstructorFunction.
static_assert(TokenConstructorFunction<TokenConstructor>,
              "Mismatch in TokenConstructorFunction (concept) and "
              "TokenConstructor, which should always satisfy it.");

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

using TokenRule = std::optional<TokenConstructor> (*)(Lexer &lexer,
                                                      location &location);

template <TokenConstructorFunction tokenConstructor, StringLiteral string>
std::optional<TokenConstructor> basicTokenRule(Lexer &lexer,
                                               location &location) {
  for (char c : string.data) {
    if (lexer.nextCharacter() != c) {
      lexer.unreadCharacter(c);
      return std::nullopt;
    }
  }
  auto tokenLocation = location;
  return [=] { return tokenConstructor(tokenLocation); };
}

std::optional<TokenConstructor> identifierTokenRule(Lexer &lexer,
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
  auto tokenLocation = location;
  return [=] { return Parser::make_IDENTIFIER(identifier, tokenLocation); };
}

std::optional<TokenConstructor> integerLiteralTokenRule(Lexer &lexer,
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
  auto tokenLocation = location;
  return [=] {
    return Parser::make_INTEGER_LITERAL(std::stoll(integerLiteral),
                                        tokenLocation);
  };
}

std::optional<TokenConstructor> stringLiteralTokenRule(Lexer &lexer,
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
  auto tokenLocation = location;
  return
      [=] { return Parser::make_STRING_LITERAL(stringLiteral, tokenLocation); };
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
  std::vector<std::pair<TokenConstructor, location>> possibleTokens;
  // Reverse the end and begin of the location so the end of the last token is
  // the begin of the new one.
  currentLocation.begin = currentLocation.end;
  location originalLocation = currentLocation;
  size_t originalBufferIndex = bufferIndex;
  for (const auto &tokenRule : tokenRules) {
    auto tokenConstructor = tokenRule(*this, currentLocation);
    if (tokenConstructor) {
      possibleTokens.push_back({std::move(*tokenConstructor), currentLocation});
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
    currentLocation = *bestLocation;
    return bestMatch->first();
  }
}

} // namespace pyrite
