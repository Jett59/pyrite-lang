#ifndef PYRITE_AST_H
#define PYRITE_AST_H

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace pyrite {
enum class AstNodeType {
  COMPILATION_UNIT,
  VARIABLE_DEFINITION,
  FUNCTION_DEFINITION,
  INTEGER_LITERAL,
  RETURN_STATEMENT
};
class AstNode {
public:
  AstNode(AstNodeType nodeType) : nodeType(nodeType) {}
  virtual ~AstNode() = default;

  AstNodeType getNodeType() const { return nodeType; }

private:
  AstNodeType nodeType;
};
class CompilationUnitNode : public AstNode {
public:
  CompilationUnitNode(std::vector<std::unique_ptr<AstNode>> definitions)
      : AstNode(AstNodeType::COMPILATION_UNIT),
        definitions(std::move(definitions)) {}

  const std::vector<std::unique_ptr<AstNode>> &getDefinitions() const {
    return definitions;
  }

private:
  std::vector<std::unique_ptr<AstNode>> definitions;
};
class VariableDefinitionNode : public AstNode {
public:
  VariableDefinitionNode(std::string name, std::unique_ptr<AstNode> initializer,
                         bool isMutable)
      : AstNode(AstNodeType::VARIABLE_DEFINITION), name(std::move(name)),
        initializer(std::move(initializer)), isMutable(isMutable) {}

  const std::string &getName() const { return name; }
  const std::unique_ptr<AstNode> &getInitializer() const { return initializer; }
  bool getIsMutable() const { return isMutable; }

private:
  std::string name;
  std::unique_ptr<AstNode> initializer;
  bool isMutable;
};
class IntegerLiteralNode : public AstNode {
public:
  IntegerLiteralNode(int64_t value)
      : AstNode(AstNodeType::INTEGER_LITERAL), value(value) {}

  int64_t getValue() const { return value; }

private:
  int64_t value;
};
} // namespace pyrite

#endif