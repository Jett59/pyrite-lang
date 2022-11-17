#include "ast.h"
#include "error.h"
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
  std::string visitDereference(const DereferenceNode &node) {
    return "*" + possiblyAddParens(visit(*node.getValue()), node.getMetadata());
  }
  std::string visitCast(const CastNode &node) {
    return possiblyAddParens(visit(*node.getValue()), node.getMetadata()) +
           " as " + typeToString(*node.getType());
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

class TypeCheckTransformer : public AstToAstTransformVisitor {
private:
  AstMetadata cloneMetadata(const AstNode &node) {
    return node.getMetadata().clone();
  }
  AstMetadata setType(const AstMetadata &original, std::unique_ptr<Type> type) {
    AstMetadata result = original.clone();
    result.valueType = std::move(type);
    return result;
  }

public:
  ValueType visitCompilationUnit(const CompilationUnitNode &node) override {
    symbolTable.push_back({});
    std::vector<std::unique_ptr<AstNode>> newDefinitions;
    for (auto &definition : node.getDefinitions()) {
      newDefinitions.push_back(visit(*definition));
    }
    symbolTable.pop_back();
    return std::make_unique<CompilationUnitNode>(std::move(newDefinitions),
                                                 cloneMetadata(node));
  }
  ValueType
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    const auto &type = *node.getType();
    auto initializer = visit(*node.getInitializer());
    const auto &initializerType = **initializer->getMetadata().valueType;
    convertTypesForAssignment(initializer, type, initializerType);
    auto valueType = cloneType(type);
    if (valueType->getTypeClass() != TypeClass::REFERENCE) {
      valueType = std::make_unique<ReferenceType>(std::move(valueType),
                                                  !node.getMutable());
    } else if (node.getMutable()) {
      throw PyriteException("Variable of reference type may not be mutable",
                            node.getMetadata());
    }
    auto result = std::make_unique<VariableDefinitionNode>(
        cloneType(type), node.getName(), std::move(initializer),
        node.getMutable(), setType(node.getMetadata(), std::move(valueType)));
    symbolTable.back().insert({node.getName(), *result});
    return std::move(result);
  }
  ValueType
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    auto returnType = cloneType(*node.getReturnType());
    std::vector<std::unique_ptr<Type>> parameterTypes;
    std::vector<NameAndType> newParameters;
    for (auto &parameter : node.getParameters()) {
      newParameters.push_back(
          NameAndType{parameter.name, cloneType(*parameter.type)});
      parameterTypes.push_back(cloneType(*parameter.type));
    }
    auto body = visit(*node.getBody());
    auto result = std::make_unique<FunctionDefinitionNode>(
        node.getName(), std::move(newParameters),
        cloneType(*node.getReturnType()), std::move(body),
        setType(node.getMetadata(),
                std::make_unique<ReferenceType>(
                    std::make_unique<FunctionType>(std::move(returnType),
                                                   std::move(parameterTypes)),
                    true)));
    symbolTable.back().insert({node.getName(), *result});
    return std::move(result);
  }
  ValueType visitIntegerLiteral(const IntegerLiteralNode &node) override {
    return std::make_unique<IntegerLiteralNode>(
        node.getValue(),
        setType(node.getMetadata(), std::make_unique<IntegerType>(32, true)));
  }
  ValueType visitFloatLiteral(const FloatLiteralNode &node) override {
    return std::make_unique<FloatLiteralNode>(
        node.getValue(),
        setType(node.getMetadata(), std::make_unique<FloatType>(64)));
  }
  ValueType visitStringLiteral(const StringLiteralNode &node) override {
    return std::make_unique<StringLiteralNode>(
        node.getValue(), setType(node.getMetadata(),
                                 std::make_unique<ArrayType>(
                                     std::make_unique<IntegerType>(8, true))));
  }
  ValueType visitBooleanLiteral(const BooleanLiteralNode &node) override {
    return std::make_unique<BooleanLiteralNode>(
        node.getValue(),
        setType(node.getMetadata(), std::make_unique<BooleanType>()));
  }
  ValueType visitCharLiteral(const CharLiteralNode &node) override {
    return std::make_unique<CharLiteralNode>(
        node.getValue(),
        setType(node.getMetadata(), std::make_unique<CharType>()));
  }
  ValueType visitReturnStatement(const ReturnStatementNode &node) override {
    auto value = visit(*node.getExpression());
    return std::make_unique<ReturnStatementNode>(std::move(value),
                                                 cloneMetadata(node));
  }
  ValueType visitBlockStatement(const BlockStatementNode &node) override {
    symbolTable.push_back({});
    std::vector<std::unique_ptr<AstNode>> newStatements;
    for (auto &statement : node.getStatements()) {
      newStatements.push_back(visit(*statement));
    }
    symbolTable.pop_back();
    return std::make_unique<BlockStatementNode>(std::move(newStatements),
                                                cloneMetadata(node));
  }
  ValueType visitIfStatement(const IfStatementNode &node) override {
    auto condition = visit(*node.getCondition());
    auto thenStatement = visit(*node.getThenStatement());
    std::unique_ptr<AstNode> elseStatement;
    if (node.getElseStatement()) {
      elseStatement = visit(*node.getElseStatement());
    }
    return std::make_unique<IfStatementNode>(
        std::move(condition), std::move(thenStatement),
        std::move(elseStatement), cloneMetadata(node));
  }
  ValueType visitWhileStatement(const WhileStatementNode &node) override {
    auto condition = visit(*node.getCondition());
    auto body = visit(*node.getBody());
    return std::make_unique<WhileStatementNode>(
        std::move(condition), std::move(body), cloneMetadata(node));
  }
  ValueType visitBinaryExpression(const BinaryExpressionNode &node) override {
    auto lhs = visit(*node.getLeft());
    auto rhs = visit(*node.getRight());
    convertTypesForBinaryOperator(lhs, rhs, **lhs->getMetadata().valueType,
                                  **rhs->getMetadata().valueType,
                                  node.getMetadata());
    auto valueType = cloneType(**lhs->getMetadata().valueType);
    return std::make_unique<BinaryExpressionNode>(
        node.getOp(), std::move(lhs), std::move(rhs),
        setType(node.getMetadata(), std::move(valueType)));
  }
  ValueType visitUnaryExpression(const UnaryExpressionNode &node) override {
    auto operand = visit(*node.getOperand());
    convertTypesForUnaryOperator(operand, **operand->getMetadata().valueType,
                                 node);
    return std::make_unique<UnaryExpressionNode>(
        node.getOp(), std::move(operand), cloneMetadata(node));
  }
  ValueType visitVariableReference(const VariableReferenceNode &node) override {
    const auto &name = node.getName();
    // Go through the symbol table backwards to get the most recent definition.
    for (auto symbolTableLevel = symbolTable.rbegin();
         symbolTableLevel != symbolTable.rend(); ++symbolTableLevel) {
      auto symbolTableEntry = symbolTableLevel->find(name);
      if (symbolTableEntry != symbolTableLevel->end()) {
        const auto &definition = symbolTableEntry->second;
        return std::make_unique<VariableReferenceNode>(
            name, setType(node.getMetadata(),
                          cloneType(**definition.getMetadata().valueType)));
      }
    }
    throw PyriteException(name + " is not defined", node.getMetadata());
  }
  ValueType visitFunctionCall(const FunctionCallNode &node) override {
    auto newFunction = visit(*node.getFunction());
    std::vector<std::unique_ptr<AstNode>> newArguments;
    for (auto &argument : node.getArguments()) {
      newArguments.push_back(visit(*argument));
    }
    const auto &functionValueType = **newFunction->getMetadata().valueType;
    if (functionValueType.getTypeClass() != TypeClass::FUNCTION) {
      throw PyriteException("Cannot call a non-function", node.getMetadata());
    }
    const auto &functionType =
        static_cast<const FunctionType &>(functionValueType);
    auto newReturnType = cloneType(functionType.getReturnType());
    return std::make_unique<FunctionCallNode>(
        std::move(newFunction), std::move(newArguments),
        setType(node.getMetadata(), std::move(newReturnType)));
  }
  ValueType visitAssignment(const AssignmentNode &node) override {
    auto newLhs = visit(*node.getLhs());
    auto newRhs = visit(*node.getRhs());
    convertTypesForAssignment(newRhs, **newLhs->getMetadata().valueType,
                              **newRhs->getMetadata().valueType);
    auto valueType = cloneType(**newLhs->getMetadata().valueType);
    return std::make_unique<AssignmentNode>(
        std::move(newLhs), std::move(newRhs), node.getAdditionalOperator(),
        setType(node.getMetadata(), std::move(valueType)));
  }
  ValueType visitDereference(const DereferenceNode &node) override {
    auto newReference = visit(*node.getValue());
    const auto &referenceValueType = **newReference->getMetadata().valueType;
    if (referenceValueType.getTypeClass() != TypeClass::REFERENCE) {
      throw PyriteException("Cannot dereference a non-reference of type " +
                                typeToString(referenceValueType),
                            node.getMetadata());
    }
    const auto &referenceType =
        static_cast<const ReferenceType &>(referenceValueType);
    auto newValueType = cloneType(referenceType.getReferencedType());
    return std::make_unique<DereferenceNode>(
        std::move(newReference),
        setType(node.getMetadata(), std::move(newValueType)));
  }
  ValueType visitCast(const CastNode &node) override {
    auto newValue = visit(*node.getValue());
    const auto &originalValueType = **newValue->getMetadata().valueType;
    auto newType = cloneType(*node.getType());
    return std::make_unique<CastNode>(
        std::move(newValue), std::move(newType),
        setType(node.getMetadata(), std::move(newType)));
  }

private:
  std::vector<std::map<std::string, const AstNode &>> symbolTable;
};

std::unique_ptr<AstNode> typeCheck(const AstNode &ast) {
  TypeCheckTransformer transformer;
  return transformer.visit(ast);
}
} // namespace pyrite
