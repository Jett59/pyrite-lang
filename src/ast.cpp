#include "ast.h"
#include "error.h"
#include <limits>
#include <string>

namespace pyrite {
std::unique_ptr<AstNode> cloneAst(const AstNode &ast) {
  PartialAstToAstTransformerVisitor visitor;
  return visitor.visit(ast);
}

class AstToStringTransformer : public AstTransformerVisitor<std::string> {
public:
  std::string visitAll(std::string value, const AstNode &node) override {
    if (node.getMetadata().valueType) {
      return "(" + value + ")" + "(" +
             typeToString(**node.getMetadata().valueType) + ")";
    } else {
      return value;
    }
  }

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
    if (node.getExpression()) {
      return "return " + visit(**node.getExpression()) + ";";
    } else {
      return "return;";
    }
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
  std::string visitArrayLiteral(const ArrayLiteralNode &node) override {
    std::string result = "[";
    if (node.getValues().size() > 0) {
      for (const auto &value : node.getValues()) {
        result += visit(*value) + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += "]";
    return result;
  }
  std::string visitStructLiteral(const StructLiteralNode &node) override {
    std::string result = "{";
    if (node.getValues().size() > 0) {
      for (const auto &value : node.getValues()) {
        result += value.first + " = " + visit(*value.second) + ", ";
      }
      result = result.substr(0, result.size() - 3);
    }
    result += "}";
    return result;
  }
  std::string visitArrayIndex(const ArrayIndexNode &node) override {
    return possiblyAddParens(visit(*node.getArray()),
                             node.getArray()->getMetadata()) +
           "[" + visit(*node.getIndex()) + "]";
  }
  std::string visitStructMember(const StructMemberNode &node) override {
    return possiblyAddParens(visit(*node.getStructValue()),
                             node.getStructValue()->getMetadata()) +
           "." + node.getMember();
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
  AstMetadata modifyMetadata(const AstMetadata &original,
                             std::optional<std::unique_ptr<Type>> type,
                             bool alwaysReturns) {
    AstMetadata result = original.clone();
    result.valueType = std::move(type);
    result.alwaysReturns = alwaysReturns;
    return result;
  }
  AstMetadata modifyMetadata(const AstNode &node,
                             std::optional<std::unique_ptr<Type>> type,
                             bool alwaysReturns) {
    return modifyMetadata(node.getMetadata(), std::move(type), alwaysReturns);
  }
  AstMetadata modifyMetadata(const AstMetadata &original,
                             std::unique_ptr<Type> type) {
    return modifyMetadata(original, std::move(type), original.alwaysReturns);
  }
  AstMetadata modifyMetadata(const AstNode &node, std::unique_ptr<Type> type) {
    return modifyMetadata(node.getMetadata(), std::move(type));
  }
  AstMetadata modifyMetadata(const AstMetadata &original, bool alwaysReturns) {
    std::optional<std::unique_ptr<Type>> type;
    if (original.valueType) {
      type = cloneType(**original.valueType);
    }
    return modifyMetadata(original, std::move(type), alwaysReturns);
  }
  AstMetadata modifyMetadata(const AstNode &node, bool alwaysReturns) {
    return modifyMetadata(node.getMetadata(), alwaysReturns);
  }
  AstMetadata cloneMetadata(const AstNode &node) {
    return node.getMetadata().clone();
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

  std::unique_ptr<Type>
  getVariableDefinitionValueType(std::unique_ptr<Type> type, bool isConstant,
                                 const AstMetadata &metadata) {
    if (type->getTypeClass() == TypeClass::REFERENCE) {
      if (!isConstant) {
        errors.push_back(
            PyriteError("Reference variable must be constant", metadata));
      }
      return type;
    } else {
      return std::make_unique<ReferenceType>(std::move(type), isConstant);
    }
  }

  ValueType
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    const auto &type = *node.getType();
    auto initializer = visit(*node.getInitializer());
    const auto &initializerType = **initializer->getMetadata().valueType;
    convertTypesForAssignment(initializer, type, initializerType);
    auto valueType = getVariableDefinitionValueType(
        cloneType(type), !node.getMutable(), node.getMetadata());
    auto result = std::make_unique<VariableDefinitionNode>(
        cloneType(type), node.getName(), std::move(initializer),
        node.getMutable(), modifyMetadata(node, std::move(valueType)));
    symbolTable.back().insert({node.getName(), *result});
    return result;
  }
  ValueType
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    symbolTable.push_back({});
    auto returnType = cloneType(*node.getReturnType());
    std::vector<std::unique_ptr<Type>> parameterTypes;
    std::vector<NameAndType> newParameters;
    std::vector<std::unique_ptr<VariableDefinitionNode>>
        parameterVariables; // Houses the values in the symbol table.
    for (auto &parameter : node.getParameters()) {
      newParameters.push_back(
          NameAndType{parameter.name, cloneType(*parameter.type)});
      parameterTypes.push_back(cloneType(*parameter.type));
      AstMetadata parameterVariableMetadata;
      parameterVariableMetadata.valueType = getVariableDefinitionValueType(
          cloneType(*parameter.type), true, parameterVariableMetadata);
      parameterVariables.push_back(std::make_unique<VariableDefinitionNode>(
          cloneType(*parameter.type), parameter.name,
          std::unique_ptr<AstNode>(nullptr), false,
          std::move(parameterVariableMetadata)));
      symbolTable.back().insert({parameter.name, *parameterVariables.back()});
    }
    parentFunctions.push_back(&node);
    auto body = visit(*node.getBody());
    if (!body->getMetadata().alwaysReturns) {
      if (returnType->getTypeClass() == TypeClass::VOID &&
          body->getNodeType() == AstNodeType::BLOCK_STATEMENT) {
        auto blockStatement = static_cast<BlockStatementNode *>(body.get());
        blockStatement->getStatements().push_back(
            std::make_unique<ReturnStatementNode>(std::nullopt,
                                                  modifyMetadata(node, true)));
        body->getMetadata().alwaysReturns = true;
      } else {
        errors.push_back(PyriteError("Function does not always return a value",
                                     node.getMetadata()));
      }
    }
    auto result = std::make_unique<FunctionDefinitionNode>(
        node.getName(), std::move(newParameters),
        cloneType(*node.getReturnType()), std::move(body),
        modifyMetadata(
            node, std::make_unique<ReferenceType>(
                      std::make_unique<FunctionType>(std::move(returnType),
                                                     std::move(parameterTypes)),
                      true)));
    symbolTable.pop_back();
    symbolTable.back().insert({node.getName(), *result});
    return std::move(result);
  }
  ValueType visitIntegerLiteral(const IntegerLiteralNode &node) override {
    bool needsI64 = node.getValue() > std::numeric_limits<int32_t>::max();
    return std::make_unique<IntegerLiteralNode>(
        node.getValue(), modifyMetadata(node, std::make_unique<IntegerType>(
                                                  needsI64 ? 64 : 32, true)));
  }
  ValueType visitFloatLiteral(const FloatLiteralNode &node) override {
    return std::make_unique<FloatLiteralNode>(
        node.getValue(), modifyMetadata(node, std::make_unique<FloatType>(64)));
  }
  ValueType visitStringLiteral(const StringLiteralNode &node) override {
    return std::make_unique<StringLiteralNode>(
        node.getValue(),
        modifyMetadata(node, std::make_unique<ArrayType>(
                                 std::make_unique<IntegerType>(8, true))));
  }
  ValueType visitBooleanLiteral(const BooleanLiteralNode &node) override {
    return std::make_unique<BooleanLiteralNode>(
        node.getValue(), modifyMetadata(node, std::make_unique<BooleanType>()));
  }
  ValueType visitCharLiteral(const CharLiteralNode &node) override {
    return std::make_unique<CharLiteralNode>(
        node.getValue(), modifyMetadata(node, std::make_unique<CharType>()));
  }
  ValueType visitReturnStatement(const ReturnStatementNode &node) override {
    std::optional<ValueType> newValue = std::nullopt;
    if (parentFunctions.size() < 1) {
      errors.push_back(PyriteError("Return statement outside of function",
                                   node.getMetadata()));
    } else {
      const FunctionDefinitionNode &parentFunction = *parentFunctions.back();
      if (node.getExpression()) {
        newValue = visit(**node.getExpression());
        if (parentFunction.getReturnType()->getTypeClass() == TypeClass::VOID) {
          errors.push_back(PyriteError("Void function may not return a value",
                                       node.getMetadata()));
        } else {
          convertTypesForAssignment(*newValue, *parentFunction.getReturnType(),
                                    **(*newValue)->getMetadata().valueType);
        }
      } else {
        if (parentFunction.getReturnType()->getTypeClass() != TypeClass::VOID) {
          errors.push_back(PyriteError("Return statement must return a value",
                                       node.getMetadata()));
        }
      }
    }
    return std::make_unique<ReturnStatementNode>(std::move(newValue),
                                                 modifyMetadata(node, true));
  }
  ValueType visitBlockStatement(const BlockStatementNode &node) override {
    symbolTable.push_back({});
    std::vector<std::unique_ptr<AstNode>> newStatements;
    bool alwaysReturns = false;
    for (auto &statement : node.getStatements()) {
      if (alwaysReturns) {
        errors.push_back(
            PyriteError("Unreachable code", statement->getMetadata()));
        break;
      }
      newStatements.push_back(visit(*statement));
      if (newStatements.back()->getMetadata().alwaysReturns) {
        alwaysReturns = true;
      }
    }
    symbolTable.pop_back();
    return std::make_unique<BlockStatementNode>(
        std::move(newStatements), modifyMetadata(node, alwaysReturns));
  }
  ValueType visitIfStatement(const IfStatementNode &node) override {
    auto condition = visit(*node.getCondition());
    auto thenStatement = visit(*node.getThenStatement());
    bool alwaysReturns = false;
    std::unique_ptr<AstNode> elseStatement;
    if (node.getElseStatement()) {
      elseStatement = visit(*node.getElseStatement());
      alwaysReturns = thenStatement->getMetadata().alwaysReturns &&
                      elseStatement->getMetadata().alwaysReturns;
    }
    return std::make_unique<IfStatementNode>(
        std::move(condition), std::move(thenStatement),
        std::move(elseStatement), modifyMetadata(node, alwaysReturns));
  }
  ValueType visitWhileStatement(const WhileStatementNode &node) override {
    auto condition = visit(*node.getCondition());
    auto body = visit(*node.getBody());
    if (body->getMetadata().alwaysReturns) {
      errors.push_back(
          PyriteError("Loop returns on first iteration", node.getMetadata()));
    }
    return std::make_unique<WhileStatementNode>(
        std::move(condition), std::move(body), cloneMetadata(node));
  }
  ValueType visitBinaryExpression(const BinaryExpressionNode &node) override {
    auto lhs = visit(*node.getLeft());
    auto rhs = visit(*node.getRight());
    convertTypesForBinaryOperator(lhs, rhs, **lhs->getMetadata().valueType,
                                  **rhs->getMetadata().valueType,
                                  node.getMetadata());
    auto valueType = isComparisonOperator(node.getOp())
                         ? std::make_unique<BooleanType>()
                         : cloneType(**lhs->getMetadata().valueType);
    return std::make_unique<BinaryExpressionNode>(
        node.getOp(), std::move(lhs), std::move(rhs),
        modifyMetadata(node, std::move(valueType)));
  }
  ValueType visitUnaryExpression(const UnaryExpressionNode &node) override {
    auto operand = visit(*node.getOperand());
    convertTypesForUnaryOperator(operand, **operand->getMetadata().valueType,
                                 node);
    return std::make_unique<UnaryExpressionNode>(
        node.getOp(), std::move(operand),
        modifyMetadata(node, cloneType(**operand->getMetadata().valueType)));
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
            name, modifyMetadata(
                      node, cloneType(**definition.getMetadata().valueType)));
      }
    }
    errors.push_back(PyriteError(name + " is not defined", node.getMetadata()));
    return std::make_unique<VariableReferenceNode>(
        name, modifyMetadata(node, std::make_unique<VoidType>()));
  }
  ValueType visitFunctionCall(const FunctionCallNode &node) override {
    auto newFunction = visit(*node.getFunction());
    std::vector<std::unique_ptr<AstNode>> newArguments;
    for (auto &argument : node.getArguments()) {
      newArguments.push_back(visit(*argument));
    }
    const auto &functionValueType = **newFunction->getMetadata().valueType;
    if (functionValueType.getTypeClass() != TypeClass::REFERENCE) {
      errors.push_back(PyriteError("Cannot call a value of type " +
                                       typeToString(functionValueType),
                                   node.getMetadata()));
      return std::make_unique<FunctionCallNode>(
          std::move(newFunction), std::move(newArguments),
          modifyMetadata(node, std::make_unique<VoidType>()));
    }
    const auto &functionReferenceType =
        static_cast<const ReferenceType &>(functionValueType);
    if (functionReferenceType.getReferencedType().getTypeClass() !=
        TypeClass::FUNCTION) {
      errors.push_back(PyriteError(
          "Cannot call a value of type " +
              typeToString(functionReferenceType.getReferencedType()),
          node.getMetadata()));
      return std::make_unique<FunctionCallNode>(
          std::move(newFunction), std::move(newArguments),
          modifyMetadata(node, std::make_unique<VoidType>()));
    }
    const auto &functionType = static_cast<const FunctionType &>(
        functionReferenceType.getReferencedType());
    if (newArguments.size() != functionType.getParameters().size()) {
      errors.push_back(PyriteError(
          "Incorrect number of arguments to function call: expected " +
              std::to_string(functionType.getParameters().size()) + ", got " +
              std::to_string(newArguments.size()),
          node.getMetadata()));
    } else {
      for (size_t i = 0; i < newArguments.size(); ++i) {
        convertTypesForAssignment(newArguments[i],
                                  *functionType.getParameters()[i],
                                  **newArguments[i]->getMetadata().valueType);
      }
    }
    auto newReturnType = cloneType(functionType.getReturnType());
    return std::make_unique<FunctionCallNode>(
        std::move(newFunction), std::move(newArguments),
        modifyMetadata(node, std::move(newReturnType)));
  }
  ValueType visitAssignment(const AssignmentNode &node) override {
    auto newLhs = visit(*node.getLhs());
    auto newRhs = visit(*node.getRhs());
    convertTypesForAssignment(newRhs, **newLhs->getMetadata().valueType,
                              **newRhs->getMetadata().valueType);
    auto valueType = cloneType(**newLhs->getMetadata().valueType);
    return std::make_unique<AssignmentNode>(
        std::move(newLhs), std::move(newRhs), node.getAdditionalOperator(),
        modifyMetadata(node, std::move(valueType)));
  }
  ValueType visitDereference(const DereferenceNode &node) override {
    auto newReference = visit(*node.getValue());
    const auto &referenceValueType = **newReference->getMetadata().valueType;
    if (referenceValueType.getTypeClass() != TypeClass::REFERENCE) {
      errors.push_back(
          PyriteError("Cannot dereference a non-reference of type " +
                          typeToString(referenceValueType),
                      node.getMetadata()));
      return std::make_unique<DereferenceNode>(
          std::move(newReference),
          modifyMetadata(node, cloneType(referenceValueType)));
    }
    const auto &referenceType =
        static_cast<const ReferenceType &>(referenceValueType);
    auto newValueType = cloneType(referenceType.getReferencedType());
    return std::make_unique<DereferenceNode>(
        std::move(newReference), modifyMetadata(node, std::move(newValueType)));
  }
  ValueType visitCast(const CastNode &node) override {
    auto newValue = visit(*node.getValue());
    const auto &originalValueType = **newValue->getMetadata().valueType;
    auto newType = cloneType(*node.getType());
    return std::make_unique<CastNode>(std::move(newValue), std::move(newType),
                                      modifyMetadata(node, std::move(newType)));
  }
  ValueType visitArrayLiteral(const ArrayLiteralNode &node) override {
    std::vector<std::unique_ptr<AstNode>> newValues;
    for (auto &value : node.getValues()) {
      newValues.push_back(visit(*value));
    }
    // Just say that we have an array of auto. The type converter will pick up
    // that we are an array literal and fix it.
    return std::make_unique<ArrayLiteralNode>(
        std::move(newValues),
        modifyMetadata(
            node, std::make_unique<ArrayType>(std::make_unique<AutoType>())));
  }
  ValueType visitStructLiteral(const StructLiteralNode &node) override {
    std::map<std::string, ValueType> newValues;
    for (auto &[name, value] : node.getValues()) {
      newValues[name] = visit(*value);
    }
    // Create a dummy struct type with what we have right now. The type
    // converter will pick up that we are a struct literal and fix it.
    std::vector<NameAndType> fields;
    for (auto &[name, value] : newValues) {
      fields.push_back({name, cloneType(**value->getMetadata().valueType)});
    }
    return std::make_unique<StructLiteralNode>(
        std::move(newValues),
        modifyMetadata(node, std::make_unique<StructType>(std::move(fields),
                                                          "<Unknown>")));
  }
  ValueType visitArrayIndex(const ArrayIndexNode &node) override {
    auto newArray = visit(*node.getArray());
    auto newIndex = visit(*node.getIndex());
    removeReference(**newArray->getMetadata().valueType, newArray);
    const auto &arrayValueType = **newArray->getMetadata().valueType;
    if (arrayValueType.getTypeClass() != TypeClass::ARRAY) {
      errors.push_back(PyriteError("Cannot index a non-array of type " +
                                       typeToString(arrayValueType),
                                   node.getMetadata()));
      return std::make_unique<ArrayIndexNode>(
          std::move(newArray), std::move(newIndex),
          modifyMetadata(node, cloneType(arrayValueType)));
    }
    const auto &arrayType = static_cast<const ArrayType &>(arrayValueType);
    convertTypesForAssignment(newIndex, IntegerType{64, false},
                              **newIndex->getMetadata().valueType);
    auto newValueType = cloneType(arrayType.getElementType());
    return std::make_unique<ArrayIndexNode>(
        std::move(newArray), std::move(newIndex),
        modifyMetadata(node, std::move(newValueType)));
  }
  ValueType visitStructMember(const StructMemberNode &node) override {
    auto newStruct = visit(*node.getStructValue());
    const auto &structValueType = **newStruct->getMetadata().valueType;
    if (structValueType.getTypeClass() != TypeClass::REFERENCE) {
      errors.push_back(
          PyriteError("Cannot access a non-struct reference of type " +
                          typeToString(structValueType),
                      node.getMetadata()));
      return std::make_unique<StructMemberNode>(
          std::move(newStruct), node.getMember(),
          modifyMetadata(node, std::make_unique<VoidType>()));
    }
    const auto &structReferenceType =
        static_cast<const ReferenceType &>(structValueType);
    const auto &structReferencedType = structReferenceType.getReferencedType();
    if (structReferencedType.getTypeClass() != TypeClass::STRUCT) {
      errors.push_back(PyriteError("Cannot access a non-struct of type " +
                                       typeToString(structReferencedType),
                                   node.getMetadata()));
      return std::make_unique<StructMemberNode>(
          std::move(newStruct), node.getMember(),
          modifyMetadata(node, std::make_unique<VoidType>()));
    }
    const auto &structType =
        static_cast<const StructType &>(structReferencedType);
    auto memberType = structType.getMemberType(node.getMember());
    if (!memberType) {
      errors.push_back(PyriteError("Struct " + structType.getName() +
                                       " does not have a member named " +
                                       node.getMember(),
                                   node.getMetadata()));
      return std::make_unique<StructMemberNode>(
          std::move(newStruct), node.getMember(),
          modifyMetadata(node, std::make_unique<VoidType>()));
    }
    return std::make_unique<StructMemberNode>(
        std::move(newStruct), node.getMember(),
        modifyMetadata(node, cloneType(**memberType)));
  }

private:
  std::vector<std::map<std::string, const AstNode &>> symbolTable;
  std::vector<const FunctionDefinitionNode *> parentFunctions;
};

std::unique_ptr<AstNode> typeCheck(const AstNode &ast) {
  TypeCheckTransformer transformer;
  return transformer.visit(ast);
}

template <typename TypeVisitor>
requires(std::is_base_of_v<TypeToTypeTransformVisitor,
                           TypeVisitor>) class AstTypeTransformer
    : public PartialAstToAstTransformerVisitor {
public:
  AstTypeTransformer(TypeVisitor typeVisitor)
      : typeVisitor(std::move(typeVisitor)) {}

  std::unique_ptr<Type> visitType(const Type &type) override {
    return typeVisitor.visit(type);
  }

  ValueType visitAll(ValueType value) override {
    const auto &valueType = value->getMetadata().valueType;
    if (valueType) {
      value->getMetadata().valueType = visitType(**valueType);
    }
    return value;
  }

  ValueType
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    std::vector<NameAndType> newParameters;
    for (const auto &parameter : node.getParameters()) {
      newParameters.push_back({parameter.name, visitType(*parameter.type)});
    }
    auto newReturnType = visitType(*node.getReturnType());
    return std::make_unique<FunctionDefinitionNode>(
        node.getName(), std::move(newParameters), std::move(newReturnType),
        visit(*node.getBody()), node.getMetadata().clone());
  }

  ValueType
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    auto newType = visitType(*node.getType());
    return std::make_unique<VariableDefinitionNode>(
        std::move(newType), node.getName(), visit(*node.getInitializer()),
        node.getMutable(), node.getMetadata().clone());
  }

  ValueType visitCast(const CastNode &node) override {
    auto newType = visitType(*node.getType());
    return std::make_unique<CastNode>(visit(*node.getValue()),
                                      std::move(newType),
                                      node.getMetadata().clone());
  }

private:
  TypeVisitor typeVisitor;
};

class TypeIdCollector : public PartialAstVisitor {
public:
  void visit(const CastNode &node) override {
    node.getValue()->accept(*this);
    const auto &type = *node.getType();
    if (type.getTypeClass() == TypeClass::ANY ||
        type.getTypeClass() == TypeClass::UNION) {
      const auto &expressionType = **node.getValue()->getMetadata().valueType;
      if (expressionType.getTypeClass() != TypeClass::ANY &&
          expressionType.getTypeClass() != TypeClass::UNION) {
        if (!typeHasId(expressionType)) {
          typeIds.push_back({cloneType(expressionType), typeIds.size()});
        }
      }
    }
  }

  std::vector<std::pair<std::unique_ptr<Type>, int64_t>> typeIds;

  bool typeHasId(const Type &type) {
    for (const auto &typeId : typeIds) {
      if (typeEquals(*typeId.first, type)) {
        return true;
      }
    }
    return false;
  }
};

class AnyToUnionTypeTransformer : public PartialTypeToTypeTransformVisitor {
public:
  AnyToUnionTypeTransformer(const TypeIdCollector &typeIdCollector)
      : typeIdCollector(typeIdCollector) {}

  ValueType visitAny(const AnyType &) override {
    std::vector<ValueType> options;
    for (const auto &type : typeIdCollector.typeIds) {
      options.push_back(cloneType(*type.first));
    }
    return std::make_unique<UnionType>(std::move(options));
  }

private:
  const TypeIdCollector &typeIdCollector;
};

static const std::string UNION_DISCRIMINATOR_FIELD = "type";
static const std::string UNION_VALUE_FIELD = "value";

class UnionToStructTransformer : public PartialAstToAstTransformerVisitor {
  static std::unique_ptr<Type> getDescriminatorType() {
    return std::make_unique<IntegerType>(32, false);
  }
  static std::unique_ptr<Type> getRawUnionType(const UnionType &unionType) {
    std::vector<std::unique_ptr<Type>> options;
    for (const auto &option : unionType.getOptions()) {
      options.push_back(cloneType(*option));
    }
    return std::make_unique<RawUnionType>(std::move(options));
  }

  class UnionToStructTypeTransformer
      : public PartialTypeToTypeTransformVisitor {
  public:
    ValueType visitUnion(const UnionType &type) override {
      std::vector<ValueType> newOptions;
      for (const auto &option : type.getOptions()) {
        newOptions.push_back(visit(*option));
      }
      auto descriminatorType = getDescriminatorType();
      std::unique_ptr<Type> rawUnionType =
          std::make_unique<RawUnionType>(std::move(newOptions));
      std::string unionTypeName = typeToString(*rawUnionType);
      std::vector<NameAndType> memberTypes;
      memberTypes.emplace_back(UNION_DISCRIMINATOR_FIELD,
                               std::move(descriminatorType));
      memberTypes.emplace_back(UNION_VALUE_FIELD, std::move(rawUnionType));
      return std::make_unique<StructType>(std::move(memberTypes),
                                          unionTypeName);
    }
  };

public:
  UnionToStructTransformer(const TypeIdCollector &typeIdCollector)
      : typeIdCollector(typeIdCollector) {}

  ValueType visit(const AstNode &ast) {
    auto result = PartialAstToAstTransformerVisitor::visit(ast);
    AstTypeTransformer typeTransformer{UnionToStructTypeTransformer{}};
    result = typeTransformer.visit(*result);
    return result;
  }

  ValueType visitCast(const CastNode &node) override {
    if (node.getType()->getTypeClass() == TypeClass::UNION) {
      const auto &unionType = static_cast<const UnionType &>(*node.getType());
      const auto &expressionType = **node.getValue()->getMetadata().valueType;
      if (expressionType.getTypeClass() != TypeClass::UNION) {
        std::optional<ValueType> descriminator = std::nullopt;
        ValueType value = cloneAst(*node.getValue());
        for (int64_t i = 0; i < typeIdCollector.typeIds.size(); i++) {
          if (typeEquals(*typeIdCollector.typeIds[i].first, expressionType)) {
            AstMetadata descriminatorMetadata = node.getMetadata().clone();
            descriminatorMetadata.valueType = getDescriminatorType();
            descriminator = std::make_unique<IntegerLiteralNode>(
                i, std::move(descriminatorMetadata));
            break;
          }
        }
        AstMetadata castMetadata = value->getMetadata().clone();
        value = std::make_unique<CastNode>(std::move(value),
                                           getRawUnionType(unionType),
                                           std::move(castMetadata));
        std::map<std::string, ValueType> fields;
        fields.emplace(UNION_DISCRIMINATOR_FIELD, std::move(*descriminator));
        fields.emplace(UNION_VALUE_FIELD, std::move(value));
        return std::make_unique<StructLiteralNode>(std::move(fields),
                                                   node.getMetadata().clone());
      }
    }
    return cloneAst(node);
  }

private:
  const TypeIdCollector &typeIdCollector;
};

std::unique_ptr<AstNode> simplifyAst(const AstNode &ast) {
  TypeIdCollector typeIdCollector;
  ast.accept(typeIdCollector);
  AstTypeTransformer<AnyToUnionTypeTransformer> anyToUnionTransformer{
      AnyToUnionTypeTransformer{typeIdCollector}};
  UnionToStructTransformer unionToStructTransformer{typeIdCollector};
  auto result = anyToUnionTransformer.visit(ast);
  result = unionToStructTransformer.visit(*result);
  return result;
}
} // namespace pyrite
