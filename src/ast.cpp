#include "ast.h"
#include <string>

namespace pyrite {
class AstToStringTransformer : public AstTransformerVisitor<std::string> {
public:
  std::string visitCompilationUnit(const CompilationUnitNode &node) override {
    std::string result;
    for (auto &definition : node.getDefinitions()) {
      result += visit(*definition) + "\n";
    }
    result = result.substr(0, result.size() - 1);
    return result;
  }
  std::string
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    return (node.getMutable() ? "mut " : "") + typeToString(*node.getType()) +
           " " + node.getName() + " = " + visit(*node.getInitializer()) + ";";
  }
  std::string
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    std::string result =
        typeToString(*node.getReturnType()) + " " + node.getName() + "(";
    if (node.getParameters().size() > 0) {
      for (auto &parameter : node.getParameters()) {
        result += typeToString(*parameter.type) + " " + parameter.name + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += ") ";
    result += visit(*node.getBody());
    return result;
  }
  std::string visitIntegerLiteral(const IntegerLiteralNode &node) override {
    return std::to_string(node.getValue());
  }
  std::string visitFloatLiteral(const FloatLiteralNode &node) override {
    return std::to_string(node.getValue());
  }
  std::string visitStringLiteral(const StringLiteralNode &node) override {
    return "\"" + node.getValue() + "\"";
  }
  std::string visitBooleanLiteral(const BooleanLiteralNode &node) override {
    return node.getValue() ? "true" : "false";
  }
  std::string visitCharLiteral(const CharLiteralNode &node) override {
    return "'" + std::to_string(static_cast<char>(node.getValue())) + "'";
  }
  std::string visitReturnStatement(const ReturnStatementNode &node) override {
    return "return " + visit(*node.getExpression()) + ";";
  }
  std::string visitBlockStatement(const BlockStatementNode &node) override {
    std::string result = "{\n";
    increaseIndent();
    for (auto &statement : node.getStatements()) {
      result += indentString(visit(*statement));
      if (result.back() != ';' && result.back() != '}') {
        result += ";";
      }
      result += "\n";
    }
    decreaseIndent();
    result += indentString("}");
    return result;
  }
  std::string visitIfStatement(const IfStatementNode &node) override {
    std::string result = "if " + visit(*node.getCondition()) + " " +
                         visit(*node.getThenStatement());
    if (node.getElseStatement()) {
      result += " else " + visit(*node.getElseStatement());
    }
    return result;
  }
  std::string visitWhileStatement(const WhileStatementNode &node) override {
    return "while " + visit(*node.getCondition()) + " " +
           visit(*node.getBody());
  }
  std::string visitBinaryExpression(const BinaryExpressionNode &node) override {
    return possiblyAddParens(visit(*node.getLeft()),
                             node.getLeft()->getMetadata()) +
           " " + binaryOperatorToString(node.getOp()) + " " +
           possiblyAddParens(visit(*node.getRight()),
                             node.getRight()->getMetadata());
  }
  std::string visitUnaryExpression(const UnaryExpressionNode &node) override {
    if (isPrefixUnaryOperator(node.getOp())) {
      return unaryOperatorToString(node.getOp()) +
             possiblyAddParens(visit(*node.getOperand()),
                               node.getOperand()->getMetadata());
    } else {
      return possiblyAddParens(visit(*node.getOperand()),
                               node.getOperand()->getMetadata()) +
             unaryOperatorToString(node.getOp());
    }
  }
  std::string
  visitVariableReference(const VariableReferenceNode &node) override {
    return node.getName();
  }
  std::string visitFunctionCall(const FunctionCallNode &node) override {
    std::string result = possiblyAddParens(visit(*node.getFunction()),
                                           node.getFunction()->getMetadata()) +
                         "(";
    if (node.getArguments().size() > 0) {
      for (auto &argument : node.getArguments()) {
        result += visit(*argument) + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += ")";
    return result;
  }
  std::string visitAssignment(const AssignmentNode &node) {
    if (node.getAdditionalOperator()) {
      return possiblyAddParens(visit(*node.getLhs()),
                               node.getLhs()->getMetadata()) +
             " " + binaryOperatorToString(*node.getAdditionalOperator()) +
             "= " +
             possiblyAddParens(visit(*node.getRhs()),
                               node.getRhs()->getMetadata());
    } else {
      return possiblyAddParens(visit(*node.getLhs()),
                               node.getLhs()->getMetadata()) +
             " = " +
             possiblyAddParens(visit(*node.getRhs()),
                               node.getRhs()->getMetadata());
    }
  }

private:
  size_t indent = 0;

  std::string indentString(std::string str) {
    std::string result;
    for (size_t i = 0; i < indent; i++) {
      result += "  ";
    }
    result += str;
    return result;
  }

  void increaseIndent() { indent += 2; }

  void decreaseIndent() { indent -= 2; }

  std::string possiblyAddParens(std::string str, const AstMetadata &metadata) {
    if (metadata.parennedExpression) {
      return "(" + std::move(str) + ")";
    } else {
      return std::move(str);
    }
  }
};

std::string astToString(const AstNode &node) {
  AstToStringTransformer transformer;
  return transformer.visit(node);
}
} // namespace pyrite
