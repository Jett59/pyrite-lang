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
    }
    buffer += c;
    bufferIndex++;
  }
  if (c == '\n') {
    currentLocation.end.line++;
    currentLocation.end.column = 1;
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
    currentLocation.end.column = 1;
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

using TokenRule =
    std::function<std::optional<TokenConstructor>(Lexer &, location &)>;

template <typename Function>
concept TokenConstructorWithLocationFunction =
    std::is_same_v<std::invoke_result_t<Function, location &>,
                   Parser::symbol_type>;

template <TokenConstructorWithLocationFunction TokenConstructorType>
TokenRule basicTokenRule(TokenConstructorType tokenConstructor,
                         std::string string) {
  return
      [=](Lexer &lexer, location &location) -> std::optional<TokenConstructor> {
        for (char c : string) {
          if (lexer.nextCharacter() != c) {
            lexer.unreadCharacter(c);
            return std::nullopt;
          }
        }
        auto tokenLocation = location;
        return [=] { return tokenConstructor(tokenLocation); };
      };
}

static std::optional<TokenConstructor> identifierTokenRule(Lexer &lexer,
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

static std::optional<TokenConstructor>
integerLiteralTokenRule(Lexer &lexer, location &location) {
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

static std::optional<TokenConstructor>
stringLiteralTokenRule(Lexer &lexer, location &location) {
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
    basicTokenRule(Parser::make_I8, "i8"),
    basicTokenRule(Parser::make_I16, "i16"),
    basicTokenRule(Parser::make_I32, "i32"),
    basicTokenRule(Parser::make_I64, "i64"),
    basicTokenRule(Parser::make_U8, "u8"),
    basicTokenRule(Parser::make_U16, "u16"),
    basicTokenRule(Parser::make_U32, "u32"),
    basicTokenRule(Parser::make_U64, "u64"),
    basicTokenRule(Parser::make_F32, "f32"),
    basicTokenRule(Parser::make_F64, "f64"),
    basicTokenRule(Parser::make_BOOL, "bool"),
    basicTokenRule(Parser::make_CHAR, "char"),
    basicTokenRule(Parser::make_VOID, "void"),
    identifierTokenRule,
    integerLiteralTokenRule,
    stringLiteralTokenRule,
    basicTokenRule(Parser::make_PLUS, "+"),
    basicTokenRule(Parser::make_MINUS, "-"),
    basicTokenRule(Parser::make_STAR, "*"),
    basicTokenRule(Parser::make_SLASH, "/"),
    basicTokenRule(Parser::make_PERCENT, "%"),
    basicTokenRule(Parser::make_AMPERSAND, "&"),
    basicTokenRule(Parser::make_PIPE, "|"),
    basicTokenRule(Parser::make_CARET, "^"),
    basicTokenRule(Parser::make_TILDE, "~"),
    basicTokenRule(Parser::make_EQUALS, "="),
    basicTokenRule(Parser::make_BANG, "!"),
    basicTokenRule(Parser::make_QUESTION, "?"),
    basicTokenRule(Parser::make_COLON, ":"),
    basicTokenRule(Parser::make_SEMICOLON, ";"),
    basicTokenRule(Parser::make_COMMA, ","),
    basicTokenRule(Parser::make_DOT, "."),
};

struct TokenMatch {
  TokenConstructor tokenConstructor;
  location tokenLocation;
  size_t finalBufferIndex;
};

Parser::symbol_type Lexer::nextToken() {
  // Skip all whitespace characters.
  {
    char lastCharacter;
    do {
      if (file.eof()) {
        return Parser::make_YYEOF(currentLocation);
      }
    } while (isspace(lastCharacter = nextCharacter()));
    unreadCharacter(lastCharacter);
  }
  std::vector<TokenMatch> possibleTokens;
  // Reverse the end and begin of the location so the end of the last token is
  // the begin of the new one.
  currentLocation.begin = currentLocation.end;
  location originalLocation = currentLocation;
  size_t originalBufferIndex = bufferIndex;
  for (const auto &tokenRule : tokenRules) {
    auto tokenConstructor = tokenRule(*this, currentLocation);
    if (tokenConstructor) {
      possibleTokens.push_back(
          {std::move(*tokenConstructor), currentLocation, bufferIndex});
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
    auto bestBufferIndex = bufferIndex;
    for (auto &match : possibleTokens) {
      if (match.finalBufferIndex > bestBufferIndex) {
        bestMatch = &match;
        bestBufferIndex = match.finalBufferIndex;
      }
    }
    currentLocation = bestMatch->tokenLocation;
    buffer = buffer.substr(bestBufferIndex);
    bufferIndex = 0;
    return bestMatch->tokenConstructor();
  }
}
} // namespace pyrite
