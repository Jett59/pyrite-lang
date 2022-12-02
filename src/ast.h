#ifndef PYRITE_AST_H
#define PYRITE_AST_H

#include "type.h"
#include <cassert>
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
  DEREFERENCE,
  CAST,
  ARRAY_LITERAL,
  RAW_ARRAY_LITERAL,
  STRUCT_LITERAL,
  ARRAY_INDEX,
  RAW_ARRAY_INDEX,
  STRUCT_MEMBER,
  ASSERT,
  EXTERNAL_FUNCTION,
};

struct AstMetadata {
  size_t line, column;
  std::optional<std::unique_ptr<Type>> valueType;
  bool parennedExpression;
  bool alwaysReturns;

  AstMetadata clone() const {
    AstMetadata metadata;
    metadata.line = line;
    metadata.column = column;
    if (valueType.has_value()) {
      metadata.valueType = cloneType(**valueType);
    }
    metadata.parennedExpression = parennedExpression;
    metadata.alwaysReturns = alwaysReturns;
    return metadata;
  }
};

class AstNode;
class CompilationUnitNode;
class VariableDefinitionNode;
class FunctionDefinitionNode;
class IntegerLiteralNode;
class FloatLiteralNode;
class StringLiteralNode;
class BooleanLiteralNode;
class CharLiteralNode;
class ReturnStatementNode;
class BlockStatementNode;
class IfStatementNode;
class WhileStatementNode;
class BinaryExpressionNode;
class UnaryExpressionNode;
class VariableReferenceNode;
class FunctionCallNode;
class AssignmentNode;
class DereferenceNode;
class CastNode;
class ArrayLiteralNode;
class RawArrayLiteralNode;
class StructLiteralNode;
class ArrayIndexNode;
class RawArrayIndexNode;
class StructMemberNode;
class AssertNode;
class ExternalFunctionNode;

class AstVisitor {
public:
  virtual ~AstVisitor() = default;

  virtual void visit(const CompilationUnitNode &) = 0;
  virtual void visit(const VariableDefinitionNode &) = 0;
  virtual void visit(const FunctionDefinitionNode &) = 0;
  virtual void visit(const IntegerLiteralNode &) = 0;
  virtual void visit(const FloatLiteralNode &) = 0;
  virtual void visit(const StringLiteralNode &) = 0;
  virtual void visit(const BooleanLiteralNode &) = 0;
  virtual void visit(const CharLiteralNode &) = 0;
  virtual void visit(const ReturnStatementNode &) = 0;
  virtual void visit(const BlockStatementNode &) = 0;
  virtual void visit(const IfStatementNode &) = 0;
  virtual void visit(const WhileStatementNode &) = 0;
  virtual void visit(const BinaryExpressionNode &) = 0;
  virtual void visit(const UnaryExpressionNode &) = 0;
  virtual void visit(const VariableReferenceNode &) = 0;
  virtual void visit(const FunctionCallNode &) = 0;
  virtual void visit(const AssignmentNode &) = 0;
  virtual void visit(const DereferenceNode &) = 0;
  virtual void visit(const CastNode &) = 0;
  virtual void visit(const ArrayLiteralNode &) = 0;
  virtual void visit(const RawArrayLiteralNode &) = 0;
  virtual void visit(const StructLiteralNode &) = 0;
  virtual void visit(const ArrayIndexNode &) = 0;
  virtual void visit(const RawArrayIndexNode &) = 0;
  virtual void visit(const StructMemberNode &) = 0;
  virtual void visit(const AssertNode &) = 0;
  virtual void visit(const ExternalFunctionNode &) = 0;
};

class AstNode {
public:
  AstNode(AstNodeType nodeType, AstMetadata metadata)
      : nodeType(nodeType), metadata(std::move(metadata)) {}
  virtual ~AstNode() = default;

  AstNodeType getNodeType() const { return nodeType; }

  const AstMetadata &getMetadata() const { return metadata; }
  // Used for the parser to change the metadata of an expression when performing
  // transformations on them.
  AstMetadata &getMetadata() { return metadata; }

  const Type &getValueType() const { return **getMetadata().valueType; }
  bool hasValueType() const { return getMetadata().valueType.has_value(); }

  void setValueType(std::unique_ptr<Type> value) {
    getMetadata().valueType = std::move(value);
  }

  virtual void accept(AstVisitor &) const = 0;

private:
  AstNodeType nodeType;
  AstMetadata metadata;
};
class CompilationUnitNode : public AstNode {
public:
  CompilationUnitNode(std::vector<std::unique_ptr<AstNode>> definitions,
                      AstMetadata metadata)
      : AstNode(AstNodeType::COMPILATION_UNIT, std::move(metadata)),
        definitions(std::move(definitions)) {}

  const std::vector<std::unique_ptr<AstNode>> &getDefinitions() const {
    return definitions;
  }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::unique_ptr<AstNode>> definitions;
};
class VariableDefinitionNode : public AstNode {
public:
  VariableDefinitionNode(std::unique_ptr<Type> type, std::string name,
                         std::unique_ptr<AstNode> initializer, bool isMutable,
                         AstMetadata metadata)
      : AstNode(AstNodeType::VARIABLE_DEFINITION, std::move(metadata)),
        type(std::move(type)), name(std::move(name)),
        initializer(std::move(initializer)), isMutable(isMutable) {}

  const std::unique_ptr<Type> &getType() const { return type; }
  const std::string &getName() const { return name; }
  const std::unique_ptr<AstNode> &getInitializer() const { return initializer; }
  bool getMutable() const { return isMutable; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<Type> type;
  std::string name;
  std::unique_ptr<AstNode> initializer;
  bool isMutable;
};
class FunctionDefinitionNode : public AstNode {
public:
  static constexpr const char *C_EXPORT_ATTRIBUTE = "c_export";

  FunctionDefinitionNode(std::string name, std::vector<NameAndType> parameters,
                         std::unique_ptr<Type> returnType,
                         std::unique_ptr<AstNode> body,
                         std::vector<std::string> attributes,
                         AstMetadata metadata)
      : AstNode(AstNodeType::FUNCTION_DEFINITION, std::move(metadata)),
        name(std::move(name)), returnType(std::move(returnType)),
        parameters(std::move(parameters)), body(std::move(body)),
        attributes(std::move(attributes)) {
    parseAttributes();
  }

  const std::string &getName() const { return name; }
  const std::unique_ptr<Type> &getReturnType() const { return returnType; }
  const std::vector<NameAndType> &getParameters() const { return parameters; }
  const std::unique_ptr<AstNode> &getBody() const { return body; }
  bool getCExported() const { return cExported; }
  const std::vector<std::string> getAttributes() const { return attributes; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::string name;
  std::unique_ptr<Type> returnType;
  std::vector<NameAndType> parameters;
  std::unique_ptr<AstNode> body;
  bool cExported;
  std::vector<std::string> attributes;

  void parseAttributes();
};
class IntegerLiteralNode : public AstNode {
public:
  IntegerLiteralNode(int64_t value, AstMetadata metadata)
      : AstNode(AstNodeType::INTEGER_LITERAL, std::move(metadata)),
        value(value) {}

  int64_t getValue() const { return value; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  int64_t value;
};
class FloatLiteralNode : public AstNode {
public:
  FloatLiteralNode(double value, AstMetadata metadata)
      : AstNode(AstNodeType::FLOAT_LITERAL, std::move(metadata)), value(value) {
  }

  double getValue() const { return value; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  double value;
};
class StringLiteralNode : public AstNode {
public:
  StringLiteralNode(std::string value, AstMetadata metadata)
      : AstNode(AstNodeType::STRING_LITERAL, std::move(metadata)),
        value(std::move(value)) {}

  const std::string &getValue() const { return value; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::string value;
};
class BooleanLiteralNode : public AstNode {
public:
  BooleanLiteralNode(bool value, AstMetadata metadata)
      : AstNode(AstNodeType::BOOLEAN_LITERAL, std::move(metadata)),
        value(value) {}

  bool getValue() const { return value; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  bool value;
};
class CharLiteralNode : public AstNode {
public:
  // "char" is actually a 32 bit unicode codepoint.
  CharLiteralNode(int32_t value, AstMetadata metadata)
      : AstNode(AstNodeType::CHAR_LITERAL, std::move(metadata)), value(value) {}

  int32_t getValue() const { return value; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  int32_t value;
};
class ReturnStatementNode : public AstNode {
public:
  ReturnStatementNode(std::optional<std::unique_ptr<AstNode>> expression,
                      AstMetadata metadata)
      : AstNode(AstNodeType::RETURN_STATEMENT, std::move(metadata)),
        expression(std::move(expression)) {}

  const std::optional<std::unique_ptr<AstNode>> &getExpression() const {
    return expression;
  }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::optional<std::unique_ptr<AstNode>> expression;
};
class BlockStatementNode : public AstNode {
public:
  BlockStatementNode(std::vector<std::unique_ptr<AstNode>> statements,
                     AstMetadata metadata)
      : AstNode(AstNodeType::BLOCK_STATEMENT, std::move(metadata)),
        statements(std::move(statements)) {}

  const std::vector<std::unique_ptr<AstNode>> &getStatements() const {
    return statements;
  }
  std::vector<std::unique_ptr<AstNode>> &getStatements() { return statements; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::unique_ptr<AstNode>> statements;
};
class IfStatementNode : public AstNode {
public:
  IfStatementNode(std::unique_ptr<AstNode> condition,
                  std::unique_ptr<AstNode> thenStatement,
                  std::unique_ptr<AstNode> elseStatement, AstMetadata metadata)
      : AstNode(AstNodeType::IF_STATEMENT, std::move(metadata)),
        condition(std::move(condition)),
        thenStatement(std::move(thenStatement)),
        elseStatement(std::move(elseStatement)) {}

  const std::unique_ptr<AstNode> &getCondition() const { return condition; }
  const std::unique_ptr<AstNode> &getThenStatement() const {
    return thenStatement;
  }
  const std::unique_ptr<AstNode> &getElseStatement() const {
    return elseStatement;
  }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> condition;
  std::unique_ptr<AstNode> thenStatement;
  std::unique_ptr<AstNode> elseStatement;
};
class WhileStatementNode : public AstNode {
public:
  WhileStatementNode(std::unique_ptr<AstNode> condition,
                     std::unique_ptr<AstNode> body, AstMetadata metadata)
      : AstNode(AstNodeType::WHILE_STATEMENT, std::move(metadata)),
        condition(std::move(condition)), body(std::move(body)) {}

  const std::unique_ptr<AstNode> &getCondition() const { return condition; }
  const std::unique_ptr<AstNode> &getBody() const { return body; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

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
static inline std::string binaryOperatorToString(BinaryOperator op) {
  switch (op) {
  case BinaryOperator::ADD:
    return "+";
  case BinaryOperator::SUBTRACT:
    return "-";
  case BinaryOperator::MULTIPLY:
    return "*";
  case BinaryOperator::DIVIDE:
    return "/";
  case BinaryOperator::MODULO:
    return "%";
  case BinaryOperator::EQUAL:
    return "==";
  case BinaryOperator::NOT_EQUAL:
    return "!=";
  case BinaryOperator::LESS_THAN:
    return "<";
  case BinaryOperator::LESS_THAN_OR_EQUAL:
    return "<=";
  case BinaryOperator::GREATER_THAN:
    return ">";
  case BinaryOperator::GREATER_THAN_OR_EQUAL:
    return ">=";
  case BinaryOperator::LOGICAL_AND:
    return "&&";
  case BinaryOperator::LOGICAL_OR:
    return "||";
  case BinaryOperator::BITWISE_AND:
    return "&";
  case BinaryOperator::BITWISE_OR:
    return "|";
  case BinaryOperator::BITWISE_XOR:
    return "^";
  case BinaryOperator::BITWISE_SHIFT_LEFT:
    return "<<";
  case BinaryOperator::BITWISE_SHIFT_RIGHT:
    return ">>";
  }
  return "";
}

static inline bool isComparisonOperator(BinaryOperator op) {
  switch (op) {
  case BinaryOperator::EQUAL:
  case BinaryOperator::NOT_EQUAL:
  case BinaryOperator::LESS_THAN:
  case BinaryOperator::LESS_THAN_OR_EQUAL:
  case BinaryOperator::GREATER_THAN:
  case BinaryOperator::GREATER_THAN_OR_EQUAL:
    return true;
  default:
    return false;
  }
}

class BinaryExpressionNode : public AstNode {
public:
  BinaryExpressionNode(BinaryOperator op, std::unique_ptr<AstNode> left,
                       std::unique_ptr<AstNode> right, AstMetadata metadata)
      : AstNode(AstNodeType::BINARY_EXPRESSION, std::move(metadata)), op(op),
        left(std::move(left)), right(std::move(right)) {}

  BinaryOperator getOp() const { return op; }
  const std::unique_ptr<AstNode> &getLeft() const { return left; }
  const std::unique_ptr<AstNode> &getRight() const { return right; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

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
static inline bool isPrefixUnaryOperator(UnaryOperator op) {
  return op == UnaryOperator::NEGATE || op == UnaryOperator::LOGICAL_NOT ||
         op == UnaryOperator::BITWISE_NOT ||
         op == UnaryOperator::PREFIX_INCREMENT ||
         op == UnaryOperator::PREFIX_DECREMENT;
}
static inline std::string unaryOperatorToString(UnaryOperator op) {
  switch (op) {
  case UnaryOperator::NEGATE:
    return "-";
  case UnaryOperator::LOGICAL_NOT:
    return "!";
  case UnaryOperator::BITWISE_NOT:
    return "~";
  case UnaryOperator::POSTFIX_INCREMENT:
    return "++";
  case UnaryOperator::POSTFIX_DECREMENT:
    return "--";
  case UnaryOperator::PREFIX_INCREMENT:
    return "++";
  case UnaryOperator::PREFIX_DECREMENT:
    return "--";
  }
  return "";
}

class UnaryExpressionNode : public AstNode {
public:
  UnaryExpressionNode(UnaryOperator op, std::unique_ptr<AstNode> operand,
                      AstMetadata metadata)
      : AstNode(AstNodeType::UNARY_EXPRESSION, std::move(metadata)), op(op),
        operand(std::move(operand)) {}

  UnaryOperator getOp() const { return op; }
  const std::unique_ptr<AstNode> &getOperand() const { return operand; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  UnaryOperator op;
  std::unique_ptr<AstNode> operand;
};

class VariableReferenceNode : public AstNode {
public:
  VariableReferenceNode(std::string name, AstMetadata metadata)
      : AstNode(AstNodeType::VARIABLE_REFERENCE, std::move(metadata)),
        name(std::move(name)) {}

  const std::string &getName() const { return name; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::string name;
};
class FunctionCallNode : public AstNode {
public:
  FunctionCallNode(std::unique_ptr<AstNode> function,
                   std::vector<std::unique_ptr<AstNode>> arguments,
                   AstMetadata metadata)
      : AstNode(AstNodeType::FUNCTION_CALL, std::move(metadata)),
        function(std::move(function)), arguments(std::move(arguments)) {}

  const std::unique_ptr<AstNode> &getFunction() const { return function; }
  const std::vector<std::unique_ptr<AstNode>> &getArguments() const {
    return arguments;
  }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> function;
  std::vector<std::unique_ptr<AstNode>> arguments;
};
class AssignmentNode : public AstNode {
public:
  AssignmentNode(std::unique_ptr<AstNode> lhs, std::unique_ptr<AstNode> rhs,
                 std::optional<BinaryOperator> additionalOperator,
                 AstMetadata metadata)
      : AstNode(AstNodeType::ASSIGNMENT, std::move(metadata)),
        lhs(std::move(lhs)), rhs(std::move(rhs)),
        additionalOperator(additionalOperator) {}

  const std::unique_ptr<AstNode> &getLhs() const { return lhs; }
  const std::unique_ptr<AstNode> &getRhs() const { return rhs; }
  const std::optional<BinaryOperator> &getAdditionalOperator() const {
    return additionalOperator;
  }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> lhs;
  std::unique_ptr<AstNode> rhs;
  std::optional<BinaryOperator> additionalOperator;
};
class DereferenceNode : public AstNode {
public:
  DereferenceNode(std::unique_ptr<AstNode> value, AstMetadata metadata)
      : AstNode(AstNodeType::DEREFERENCE, std::move(metadata)),
        value(std::move(value)) {}

  const std::unique_ptr<AstNode> &getValue() const { return value; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> value;
};
class CastNode : public AstNode {
public:
  CastNode(std::unique_ptr<AstNode> value, std::unique_ptr<Type> type,
           AstMetadata metadata)
      : AstNode(AstNodeType::CAST, std::move(metadata)),
        value(std::move(value)), type(std::move(type)) {}

  const std::unique_ptr<AstNode> &getValue() const { return value; }
  const std::unique_ptr<Type> &getType() const { return type; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> value;
  std::unique_ptr<Type> type;
};
class RawArrayLiteralNode : public AstNode {
protected:
  RawArrayLiteralNode(std::vector<std::unique_ptr<AstNode>> values,
                      AstMetadata metadata, AstNodeType astNodeType)
      : AstNode(astNodeType, std::move(metadata)), values(std::move(values)) {}

public:
  RawArrayLiteralNode(std::vector<std::unique_ptr<AstNode>> values,
                      AstMetadata metadata)
      : AstNode(AstNodeType::RAW_ARRAY_LITERAL, std::move(metadata)),
        values(std::move(values)) {}

  const std::vector<std::unique_ptr<AstNode>> &getValues() const {
    return values;
  }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::unique_ptr<AstNode>> values;
};
class ArrayLiteralNode : public RawArrayLiteralNode {
public:
  ArrayLiteralNode(std::vector<std::unique_ptr<AstNode>> values,
                   AstMetadata metadata)
      : RawArrayLiteralNode(std::move(values), std::move(metadata),
                            AstNodeType::ARRAY_LITERAL) {}

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }
};
class StructLiteralNode : public AstNode {
public:
  StructLiteralNode(
      std::vector<std::pair<std::string, std::unique_ptr<AstNode>>> values,
      AstMetadata metadata)
      : AstNode(AstNodeType::STRUCT_LITERAL, std::move(metadata)),
        values(std::move(values)) {}

  const auto &getValues() const { return values; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::pair<std::string, std::unique_ptr<AstNode>>> values;
};
class RawArrayIndexNode : public AstNode {
protected:
  RawArrayIndexNode(std::unique_ptr<AstNode> array,
                    std::unique_ptr<AstNode> index, AstMetadata metadata,
                    AstNodeType astNodeType)
      : AstNode(astNodeType, std::move(metadata)), array(std::move(array)),
        index(std::move(index)) {}

public:
  RawArrayIndexNode(std::unique_ptr<AstNode> array,
                    std::unique_ptr<AstNode> index, AstMetadata metadata)
      : AstNode(AstNodeType::RAW_ARRAY_INDEX, std::move(metadata)),
        array(std::move(array)), index(std::move(index)) {}

  const std::unique_ptr<AstNode> &getArray() const { return array; }
  const std::unique_ptr<AstNode> &getIndex() const { return index; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> array;
  std::unique_ptr<AstNode> index;
};
class ArrayIndexNode : public RawArrayIndexNode {
public:
  ArrayIndexNode(std::unique_ptr<AstNode> array, std::unique_ptr<AstNode> index,
                 AstMetadata metadata)
      : RawArrayIndexNode(std::move(array), std::move(index),
                          std::move(metadata), AstNodeType::ARRAY_INDEX) {}

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }
};
class StructMemberNode : public AstNode {
public:
  StructMemberNode(std::unique_ptr<AstNode> structValue, std::string member,
                   AstMetadata metadata)
      : AstNode(AstNodeType::STRUCT_MEMBER, std::move(metadata)),
        structValue(std::move(structValue)), member(std::move(member)) {}

  const std::unique_ptr<AstNode> &getStructValue() const { return structValue; }
  const std::string &getMember() const { return member; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> structValue;
  std::string member;
};
/**
 * @brief assert that a condition is true
 *
 * assert that a condition is true, returning either the LHS or RHS if
 * true, otherwise running the provided panic statement.
 */
class AssertNode : public AstNode {
public:
  AssertNode(std::unique_ptr<AstNode> lhs, std::unique_ptr<AstNode> rhs,
             bool useLhs, BinaryOperator op, std::unique_ptr<AstNode> panic,
             AstMetadata metadata)
      : AstNode(AstNodeType::ASSERT, std::move(metadata)), lhs(std::move(lhs)),
        rhs(std::move(rhs)), useLhs(useLhs), op(op), panic(std::move(panic)) {
    assert(isComparisonOperator(op));
  }

  const std::unique_ptr<AstNode> &getLhs() const { return lhs; }
  const std::unique_ptr<AstNode> &getRhs() const { return rhs; }
  bool getUseLhs() const { return useLhs; }
  BinaryOperator getOp() const { return op; }
  const std::unique_ptr<AstNode> &getPanic() const { return panic; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<AstNode> lhs;
  std::unique_ptr<AstNode> rhs;
  bool useLhs;
  BinaryOperator op;
  std::unique_ptr<AstNode> panic;
};
class ExternalFunctionNode : public AstNode {
public:
  ExternalFunctionNode(std::string name, std::vector<NameAndType> parameters,
                       std::unique_ptr<Type> returnType, AstMetadata metadata)
      : AstNode(AstNodeType::EXTERNAL_FUNCTION, std::move(metadata)),
        name(std::move(name)), parameters(std::move(parameters)),
        returnType(std::move(returnType)) {}

  const std::string &getName() const { return name; }
  const std::vector<NameAndType> &getParameters() const { return parameters; }
  const std::unique_ptr<Type> &getReturnType() const { return returnType; }

  void accept(AstVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::string name;
  std::vector<NameAndType> parameters;
  std::unique_ptr<Type> returnType;
};

class PartialAstVisitor : public AstVisitor {
public:
  void visit(const CompilationUnitNode &node) override {
    for (const auto &definition : node.getDefinitions()) {
      definition->accept(*this);
    }
  }
  void visit(const VariableDefinitionNode &node) override {
    node.getInitializer()->accept(*this);
  }
  void visit(const FunctionDefinitionNode &node) override {
    node.getBody()->accept(*this);
  }
  void visit(const IntegerLiteralNode &node) override {}
  void visit(const FloatLiteralNode &node) override {}
  void visit(const StringLiteralNode &node) override {}
  void visit(const BooleanLiteralNode &node) override {}
  void visit(const CharLiteralNode &node) override {}
  void visit(const ReturnStatementNode &node) override {
    if (node.getExpression()) {
      (*node.getExpression())->accept(*this);
    }
  }
  void visit(const BlockStatementNode &node) override {
    for (const auto &statement : node.getStatements()) {
      statement->accept(*this);
    }
  }
  void visit(const IfStatementNode &node) override {
    node.getCondition()->accept(*this);
    node.getThenStatement()->accept(*this);
    if (node.getElseStatement()) {
      node.getElseStatement()->accept(*this);
    }
  }
  void visit(const WhileStatementNode &node) override {
    node.getCondition()->accept(*this);
    node.getBody()->accept(*this);
  }
  void visit(const BinaryExpressionNode &node) override {
    node.getLeft()->accept(*this);
    node.getRight()->accept(*this);
  }
  void visit(const UnaryExpressionNode &node) override {
    node.getOperand()->accept(*this);
  }
  void visit(const VariableReferenceNode &node) override {}
  void visit(const FunctionCallNode &node) override {
    node.getFunction()->accept(*this);
    for (const auto &argument : node.getArguments()) {
      argument->accept(*this);
    }
  }
  void visit(const AssignmentNode &node) override {
    node.getLhs()->accept(*this);
    node.getRhs()->accept(*this);
  }
  void visit(const DereferenceNode &node) override {
    node.getValue()->accept(*this);
  }
  void visit(const CastNode &node) override { node.getValue()->accept(*this); }
  void visit(const ArrayLiteralNode &node) override {
    for (const auto &value : node.getValues()) {
      value->accept(*this);
    }
  }
  void visit(const RawArrayLiteralNode &node) override {
    for (const auto &value : node.getValues()) {
      value->accept(*this);
    }
  }
  void visit(const StructLiteralNode &node) override {
    for (const auto &[name, value] : node.getValues()) {
      value->accept(*this);
    }
  }
  void visit(const ArrayIndexNode &node) override {
    node.getArray()->accept(*this);
    node.getIndex()->accept(*this);
  }
  void visit(const RawArrayIndexNode &node) override {
    node.getArray()->accept(*this);
    node.getIndex()->accept(*this);
  }
  void visit(const StructMemberNode &node) override {
    node.getStructValue()->accept(*this);
  }
  void visit(const AssertNode &node) override {
    node.getLhs()->accept(*this);
    node.getRhs()->accept(*this);
    node.getPanic()->accept(*this);
  }
  void visit(const ExternalFunctionNode &) override {}
};
static_assert(
    !std::is_abstract_v<PartialAstVisitor>,
    "PartialAstVisitor does not implement all of the required methods");

template <typename TemplatedValueType>
class AstTransformerVisitor : public AstVisitor {
public:
  using ValueType = TemplatedValueType;

  virtual ValueType visitAll(ValueType value) { return value; }
  virtual ValueType visitAll(ValueType value, const AstNode &node) {
    return visitAll(std::move(value));
  }

  ValueType visit(const AstNode &node) {
    node.accept(*this);
    return visitAll(std::move(result), node);
  }

  // I don't want to implement the same four lines of code for every single type
  // of ast node, so I won't. I'll use an ugly macro to do it so I just need one
  // line of code for each one. This is ugly.
#define IMPLEMENT_VISIT_NODE(FUNCTION_SUFFIX)                                  \
  virtual ValueType visit##FUNCTION_SUFFIX(const FUNCTION_SUFFIX##Node &) = 0; \
  void visit(const FUNCTION_SUFFIX##Node &node) override {                     \
    result = visit##FUNCTION_SUFFIX(node);                                     \
  }

  IMPLEMENT_VISIT_NODE(CompilationUnit)
  IMPLEMENT_VISIT_NODE(VariableDefinition)
  IMPLEMENT_VISIT_NODE(FunctionDefinition)
  IMPLEMENT_VISIT_NODE(IntegerLiteral)
  IMPLEMENT_VISIT_NODE(FloatLiteral)
  IMPLEMENT_VISIT_NODE(StringLiteral)
  IMPLEMENT_VISIT_NODE(BooleanLiteral)
  IMPLEMENT_VISIT_NODE(CharLiteral)
  IMPLEMENT_VISIT_NODE(ReturnStatement)
  IMPLEMENT_VISIT_NODE(BlockStatement)
  IMPLEMENT_VISIT_NODE(IfStatement)
  IMPLEMENT_VISIT_NODE(WhileStatement)
  IMPLEMENT_VISIT_NODE(BinaryExpression)
  IMPLEMENT_VISIT_NODE(UnaryExpression)
  IMPLEMENT_VISIT_NODE(VariableReference)
  IMPLEMENT_VISIT_NODE(FunctionCall)
  IMPLEMENT_VISIT_NODE(Assignment)
  IMPLEMENT_VISIT_NODE(Dereference)
  IMPLEMENT_VISIT_NODE(Cast)
  IMPLEMENT_VISIT_NODE(ArrayLiteral)
  IMPLEMENT_VISIT_NODE(RawArrayLiteral)
  IMPLEMENT_VISIT_NODE(StructLiteral)
  IMPLEMENT_VISIT_NODE(ArrayIndex)
  IMPLEMENT_VISIT_NODE(RawArrayIndex)
  IMPLEMENT_VISIT_NODE(StructMember)
  IMPLEMENT_VISIT_NODE(Assert)
  IMPLEMENT_VISIT_NODE(ExternalFunction)

#undef IMPLEMENT_VISIT_NODE

private:
  ValueType result;
};

using AstToAstTransformVisitor =
    AstTransformerVisitor<std::unique_ptr<AstNode>>;

class PartialAstToAstTransformerVisitor : public AstToAstTransformVisitor {
public:
  virtual std::unique_ptr<Type> visitType(const Type &type) {
    return cloneType(type);
  }

  ValueType visitCompilationUnit(const CompilationUnitNode &node) override {
    std::vector<ValueType> newDefinitions;
    for (const auto &definition : node.getDefinitions()) {
      newDefinitions.push_back(visit(*definition));
    }
    return std::make_unique<CompilationUnitNode>(std::move(newDefinitions),
                                                 node.getMetadata().clone());
  }
  ValueType
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    return std::make_unique<VariableDefinitionNode>(
        visitType(*node.getType()), node.getName(),
        visit(*node.getInitializer()), node.getMutable(),
        node.getMetadata().clone());
  }
  ValueType
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    std::vector<NameAndType> newParameters;
    for (const auto &parameter : node.getParameters()) {
      newParameters.push_back({parameter.name, visitType(*parameter.type)});
    }
    return std::make_unique<FunctionDefinitionNode>(
        node.getName(), std::move(newParameters),
        visitType(*node.getReturnType()), visit(*node.getBody()),
        node.getAttributes(), node.getMetadata().clone());
  }
  ValueType visitIntegerLiteral(const IntegerLiteralNode &node) override {
    return std::make_unique<IntegerLiteralNode>(node.getValue(),
                                                node.getMetadata().clone());
  }
  ValueType visitFloatLiteral(const FloatLiteralNode &node) override {
    return std::make_unique<FloatLiteralNode>(node.getValue(),
                                              node.getMetadata().clone());
  }
  ValueType visitStringLiteral(const StringLiteralNode &node) override {
    return std::make_unique<StringLiteralNode>(node.getValue(),
                                               node.getMetadata().clone());
  }
  ValueType visitBooleanLiteral(const BooleanLiteralNode &node) override {
    return std::make_unique<BooleanLiteralNode>(node.getValue(),
                                                node.getMetadata().clone());
  }
  ValueType visitCharLiteral(const CharLiteralNode &node) override {
    return std::make_unique<CharLiteralNode>(node.getValue(),
                                             node.getMetadata().clone());
  }
  ValueType visitReturnStatement(const ReturnStatementNode &node) override {
    bool hasExpression = node.getExpression().has_value();
    std::optional<ValueType> newExpression = std::nullopt;
    if (hasExpression) {
      newExpression = visit(*node.getExpression().value());
    }
    return std::make_unique<ReturnStatementNode>(std::move(newExpression),
                                                 node.getMetadata().clone());
  }
  ValueType visitBlockStatement(const BlockStatementNode &node) override {
    std::vector<ValueType> newStatements;
    for (const auto &statement : node.getStatements()) {
      newStatements.push_back(visit(*statement));
    }
    return std::make_unique<BlockStatementNode>(std::move(newStatements),
                                                node.getMetadata().clone());
  }
  ValueType visitIfStatement(const IfStatementNode &node) override {
    return std::make_unique<IfStatementNode>(
        visit(*node.getCondition()), visit(*node.getThenStatement()),
        node.getElseStatement() ? visit(*node.getElseStatement()) : nullptr,
        node.getMetadata().clone());
  }
  ValueType visitWhileStatement(const WhileStatementNode &node) override {
    return std::make_unique<WhileStatementNode>(visit(*node.getCondition()),
                                                visit(*node.getBody()),
                                                node.getMetadata().clone());
  }
  ValueType visitBinaryExpression(const BinaryExpressionNode &node) override {
    return std::make_unique<BinaryExpressionNode>(
        node.getOp(), visit(*node.getLeft()), visit(*node.getRight()),
        node.getMetadata().clone());
  }
  ValueType visitUnaryExpression(const UnaryExpressionNode &node) override {
    return std::make_unique<UnaryExpressionNode>(
        node.getOp(), visit(*node.getOperand()), node.getMetadata().clone());
  }
  ValueType visitVariableReference(const VariableReferenceNode &node) override {
    return std::make_unique<VariableReferenceNode>(node.getName(),
                                                   node.getMetadata().clone());
  }
  ValueType visitFunctionCall(const FunctionCallNode &node) override {
    std::vector<ValueType> newArguments;
    for (const auto &argument : node.getArguments()) {
      newArguments.push_back(visit(*argument));
    }
    return std::make_unique<FunctionCallNode>(visit(*node.getFunction()),
                                              std::move(newArguments),
                                              node.getMetadata().clone());
  }
  ValueType visitAssignment(const AssignmentNode &node) override {
    return std::make_unique<AssignmentNode>(
        visit(*node.getLhs()), visit(*node.getRhs()),
        node.getAdditionalOperator(), node.getMetadata().clone());
  }
  ValueType visitDereference(const DereferenceNode &node) override {
    return std::make_unique<DereferenceNode>(visit(*node.getValue()),
                                             node.getMetadata().clone());
  }
  ValueType visitCast(const CastNode &node) override {
    return std::make_unique<CastNode>(visit(*node.getValue()),
                                      visitType(*node.getType()),
                                      node.getMetadata().clone());
  }
  ValueType visitArrayLiteral(const ArrayLiteralNode &node) override {
    std::vector<ValueType> newValues;
    for (const auto &value : node.getValues()) {
      newValues.push_back(visit(*value));
    }
    return std::make_unique<ArrayLiteralNode>(std::move(newValues),
                                              node.getMetadata().clone());
  }
  ValueType visitRawArrayLiteral(const RawArrayLiteralNode &node) override {
    std::vector<ValueType> newValues;
    for (const auto &value : node.getValues()) {
      newValues.push_back(visit(*value));
    }
    return std::make_unique<RawArrayLiteralNode>(std::move(newValues),
                                                 node.getMetadata().clone());
  }
  ValueType visitStructLiteral(const StructLiteralNode &node) override {
    std::vector<std::pair<std::string, ValueType>> newValues;
    for (const auto &[name, value] : node.getValues()) {
      newValues.push_back({name, visit(*value)});
    }
    return std::make_unique<StructLiteralNode>(std::move(newValues),
                                               node.getMetadata().clone());
  }
  ValueType visitArrayIndex(const ArrayIndexNode &node) override {
    return std::make_unique<ArrayIndexNode>(visit(*node.getArray()),
                                            visit(*node.getIndex()),
                                            node.getMetadata().clone());
  }
  ValueType visitRawArrayIndex(const RawArrayIndexNode &node) override {
    return std::make_unique<RawArrayIndexNode>(visit(*node.getArray()),
                                               visit(*node.getIndex()),
                                               node.getMetadata().clone());
  }
  ValueType visitStructMember(const StructMemberNode &node) override {
    return std::make_unique<StructMemberNode>(visit(*node.getStructValue()),
                                              node.getMember(),
                                              node.getMetadata().clone());
  }
  ValueType visitAssert(const AssertNode &node) override {
    return std::make_unique<AssertNode>(
        visit(*node.getLhs()), visit(*node.getRhs()), node.getUseLhs(),
        node.getOp(), visit(*node.getPanic()), node.getMetadata().clone());
  }
  ValueType visitExternalFunction(const ExternalFunctionNode &node) override {
    std::vector<NameAndType> newParameters;
    for (const auto &[name, type] : node.getParameters()) {
      newParameters.push_back({name, visitType(*type)});
    }
    return std::make_unique<ExternalFunctionNode>(
        node.getName(), std::move(newParameters),
        visitType(*node.getReturnType()), node.getMetadata().clone());
  }
};
static_assert(!std::is_abstract_v<PartialAstToAstTransformerVisitor>,
              "PartialAstToAstTransformVisitor does not implement all the "
              "required methods from AstTransformVisitor");

std::unique_ptr<AstNode> cloneAst(const AstNode &node);

std::string astToString(const AstNode &);

// Assigns types to expressions, checks them against the expected types, and
// performs implicit conversions.
std::unique_ptr<AstNode> typeCheck(const AstNode &ast);
std::unique_ptr<AstNode> simplifyAst(const AstNode &ast);
} // namespace pyrite

#endif