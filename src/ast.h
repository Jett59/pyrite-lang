#ifndef PYRITE_AST_H
#define PYRITE_AST_H

#include "type.h"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace pyrite {
enum class AstNodeType {
  COMPILATION_UNIT,
  VARIABLE_DEFINITION,
  FUNCTION_DEFINITION,
  INTEGER_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,
  BOOLEAN_LITERAL,
  CHAR_LITERAL,
  RETURN_STATEMENT,
  BLOCK_STATEMENT,
  IF_STATEMENT,
  WHILE_STATEMENT,
  BINARY_EXPRESSION,
  UNARY_EXPRESSION,
  VARIABLE_REFERENCE,
  FUNCTION_CALL,
  ASSIGNMENT,
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
  VariableDefinitionNode(std::unique_ptr<Type> type, std::string name,
                         std::unique_ptr<AstNode> initializer, bool isMutable)
      : AstNode(AstNodeType::VARIABLE_DEFINITION), type(std::move(type)),
        name(std::move(name)), initializer(std::move(initializer)),
        isMutable(isMutable) {}

  const std::unique_ptr<Type> &getType() const { return type; }
  const std::string &getName() const { return name; }
  const std::unique_ptr<AstNode> &getInitializer() const { return initializer; }
  bool getIsMutable() const { return isMutable; }

private:
  std::unique_ptr<Type> type;
  std::string name;
  std::unique_ptr<AstNode> initializer;
  bool isMutable;
};
class FunctionDefinitionNode : public AstNode {
public:
  FunctionDefinitionNode(std::string name, std::vector<NameAndType> parameters,
                         std::unique_ptr<Type> returnType,
                         std::unique_ptr<AstNode> body)
      : AstNode(AstNodeType::FUNCTION_DEFINITION), name(std::move(name)),
        returnType(std::move(returnType)), parameters(std::move(parameters)),
        body(std::move(body)) {}

  const std::string &getName() const { return name; }
  const std::unique_ptr<Type> &getReturnType() const { return returnType; }
  const std::vector<NameAndType> &getParameters() const { return parameters; }
  const std::unique_ptr<AstNode> &getBody() const { return body; }

private:
  std::string name;
  std::unique_ptr<Type> returnType;
  std::vector<NameAndType> parameters;
  std::unique_ptr<AstNode> body;
};
class IntegerLiteralNode : public AstNode {
public:
  IntegerLiteralNode(int64_t value)
      : AstNode(AstNodeType::INTEGER_LITERAL), value(value) {}

  int64_t getValue() const { return value; }

private:
  int64_t value;
};
class FloatLiteralNode : public AstNode {
public:
  FloatLiteralNode(double value)
      : AstNode(AstNodeType::FLOAT_LITERAL), value(value) {}

  double getValue() const { return value; }

private:
  double value;
};
class BooleanLiteralNode : public AstNode {
public:
  BooleanLiteralNode(bool value)
      : AstNode(AstNodeType::BOOLEAN_LITERAL), value(value) {}

  bool getValue() const { return value; }

private:
  bool value;
};
class CharLiteralNode : public AstNode {
public:
  // "char" is actually a 32 bit codepoint.
  CharLiteralNode(int32_t value)
      : AstNode(AstNodeType::CHAR_LITERAL), value(value) {}

  int32_t getValue() const { return value; }

private:
  int32_t value;
};
class ReturnStatementNode : public AstNode {
public:
  ReturnStatementNode(std::unique_ptr<AstNode> expression)
      : AstNode(AstNodeType::RETURN_STATEMENT),
        expression(std::move(expression)) {}

  const std::unique_ptr<AstNode> &getExpression() const { return expression; }

private:
  std::unique_ptr<AstNode> expression;
};
class BlockStatementNode : public AstNode {
public:
  BlockStatementNode(std::vector<std::unique_ptr<AstNode>> statements)
      : AstNode(AstNodeType::BLOCK_STATEMENT),
        statements(std::move(statements)) {}

  const std::vector<std::unique_ptr<AstNode>> &getStatements() const {
    return statements;
  }

private:
  std::vector<std::unique_ptr<AstNode>> statements;
};
class IfStatementNode : public AstNode {
public:
  IfStatementNode(std::unique_ptr<AstNode> condition,
                  std::unique_ptr<AstNode> thenStatement,
                  std::unique_ptr<AstNode> elseStatement)
      : AstNode(AstNodeType::IF_STATEMENT), condition(std::move(condition)),
        thenStatement(std::move(thenStatement)),
        elseStatement(std::move(elseStatement)) {}

  const std::unique_ptr<AstNode> &getCondition() const { return condition; }
  const std::unique_ptr<AstNode> &getThenStatement() const {
    return thenStatement;
  }
  const std::unique_ptr<AstNode> &getElseStatement() const {
    return elseStatement;
  }

private:
  std::unique_ptr<AstNode> condition;
  std::unique_ptr<AstNode> thenStatement;
  std::unique_ptr<AstNode> elseStatement;
};
class WhileStatementNode : public AstNode {
public:
  WhileStatementNode(std::unique_ptr<AstNode> condition,
                     std::unique_ptr<AstNode> body)
      : AstNode(AstNodeType::WHILE_STATEMENT), condition(std::move(condition)),
        body(std::move(body)) {}

  const std::unique_ptr<AstNode> &getCondition() const { return condition; }
  const std::unique_ptr<AstNode> &getBody() const { return body; }

private:
  std::unique_ptr<AstNode> condition;
  std::unique_ptr<AstNode> body;
};

enum class BinaryOperator {
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
  MODULO,
  EQUAL,
  NOT_EQUAL,
  LESS_THAN,
  LESS_THAN_OR_EQUAL,
  GREATER_THAN,
  GREATER_THAN_OR_EQUAL,
  LOGICAL_AND,
  LOGICAL_OR,
  BITWISE_AND,
  BITWISE_OR,
  BITWISE_XOR,
  BITWISE_SHIFT_LEFT,
  BITWISE_SHIFT_RIGHT,
};

class BinaryExpressionNode : public AstNode {
public:
  BinaryExpressionNode(BinaryOperator op, std::unique_ptr<AstNode> left,
                       std::unique_ptr<AstNode> right)
      : AstNode(AstNodeType::BINARY_EXPRESSION), op(op), left(std::move(left)),
        right(std::move(right)) {}

  BinaryOperator getOp() const { return op; }
  const std::unique_ptr<AstNode> &getLeft() const { return left; }
  const std::unique_ptr<AstNode> &getRight() const { return right; }

private:
  BinaryOperator op;
  std::unique_ptr<AstNode> left;
  std::unique_ptr<AstNode> right;
};

enum class UnaryOperator {
  NEGATE,
  LOGICAL_NOT,
  BITWISE_NOT,
  POSTFIX_INCREMENT,
  POSTFIX_DECREMENT,
  PREFIX_INCREMENT,
  PREFIX_DECREMENT,
};

class UnaryExpressionNode : public AstNode {
public:
  UnaryExpressionNode(UnaryOperator op, std::unique_ptr<AstNode> operand)
      : AstNode(AstNodeType::UNARY_EXPRESSION), op(op),
        operand(std::move(operand)) {}

  UnaryOperator getOp() const { return op; }
  const std::unique_ptr<AstNode> &getOperand() const { return operand; }

private:
  UnaryOperator op;
  std::unique_ptr<AstNode> operand;
};
class VariableReferenceNode : public AstNode {
public:
  VariableReferenceNode(std::string name)
      : AstNode(AstNodeType::VARIABLE_REFERENCE), name(std::move(name)) {}

  const std::string &getName() const { return name; }

private:
  std::string name;
};
class FunctionCallNode : public AstNode {
public:
  FunctionCallNode(std::unique_ptr<AstNode> function,
                   std::vector<std::unique_ptr<AstNode>> arguments)
      : AstNode(AstNodeType::FUNCTION_CALL), function(std::move(function)),
        arguments(std::move(arguments)) {}

  const std::unique_ptr<AstNode> &getFunction() const { return function; }
  const std::vector<std::unique_ptr<AstNode>> &getArguments() const {
    return arguments;
  }

private:
  std::unique_ptr<AstNode> function;
  std::vector<std::unique_ptr<AstNode>> arguments;
};
class AssignmentNode : public AstNode {
public:
  AssignmentNode(
      std::unique_ptr<AstNode> lhs, std::unique_ptr<AstNode> rhs,
      std::optional<BinaryOperator> additionalOperator = std::nullopt)
      : AstNode(AstNodeType::ASSIGNMENT), lhs(std::move(lhs)),
        rhs(std::move(rhs)), additionalOperator(additionalOperator) {}

  const std::unique_ptr<AstNode> &getLhs() const { return lhs; }
  const std::unique_ptr<AstNode> &getRhs() const { return rhs; }
  const std::optional<BinaryOperator> &getAdditionalOperator() const {
    return additionalOperator;
  }

private:
  std::unique_ptr<AstNode> lhs;
  std::unique_ptr<AstNode> rhs;
  std::optional<BinaryOperator> additionalOperator;
};
} // namespace pyrite

#endif