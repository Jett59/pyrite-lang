#include "ast.h"
#include "error.h"
#include "escapes.h"
#include "mangle.h"
#include <atomic>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <variant>

namespace pyrite {
void FunctionDefinitionNode::parseAttributes() {
  for (const auto &attribute : attributes) {
    if (attribute == C_EXPORT_ATTRIBUTE) {
      cExported = true;
    } else {
      errors.push_back(
          PyriteError{"Unknown attribute: " + attribute, getMetadata()});
    }
  }
}

std::unique_ptr<AstNode> cloneAst(const AstNode &ast) {
  PartialAstToAstTransformerVisitor visitor;
  return visitor.visit(ast);
}

std::unique_ptr<VariableDefinitionNode>
createTemporaryVariable(std::unique_ptr<AstNode> initializer) {
  static std::atomic_int counter{0};
  auto variableMetadata = initializer->getMetadata().clone();
  variableMetadata.valueType = std::make_unique<ReferenceType>(
      cloneType(removeReference(initializer->getValueType())), true);
  return std::make_unique<VariableDefinitionNode>(
      cloneType(initializer->getValueType()),
      "$tmp" + std::to_string(counter++), std::move(initializer), false,
      std::move(variableMetadata));
}
std::unique_ptr<AstNode>
createTemporaryVariableReference(const VariableDefinitionNode &node) {
  auto variableMetadata = node.getMetadata().clone();
  variableMetadata.valueType = cloneType(node.getValueType());
  return std::make_unique<VariableReferenceNode>(node.getName(),
                                                 std::move(variableMetadata));
}

class AstToStringTransformer : public AstTransformerVisitor<std::string> {
public:
  std::string visitAll(std::string value, const AstNode &node) override {
    if (node.hasValueType()) {
      return "(" + value + ")" + "(" + typeToString(node.getValueType()) + ")";
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
    return (node.getMutable() ? "mut " : "let ") +
           typeToString(*node.getType()) + " " + node.getName() + " = " +
           visit(*node.getInitializer()) + ";";
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
  std::string visitRawArrayLiteral(const RawArrayLiteralNode &node) override {
    std::string result = "raw[";
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
        result += value.first + ": " + visit(*value.second) + ", ";
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
  std::string visitRawArrayIndex(const RawArrayIndexNode &node) override {
    return possiblyAddParens(visit(*node.getArray()),
                             node.getArray()->getMetadata()) +
           "raw[" + visit(*node.getIndex()) + "]";
  }
  std::string visitStructMember(const StructMemberNode &node) override {
    return possiblyAddParens(visit(*node.getStructValue()),
                             node.getStructValue()->getMetadata()) +
           "." + node.getMember();
  }
  std::string visitAssert(const AssertNode &node) {
    return "assert(" + visit(*node.getLhs()) + " " +
           binaryOperatorToString(node.getOp()) + " " + visit(*node.getRhs()) +
           ", " + (node.getUseLhs() ? "lhs" : "rhs") + ": " +
           visit(*node.getPanic()) + ")";
  }
  std::string visitExternalFunction(const ExternalFunctionNode &node) override {
    std::string result = "c_extern fn " + node.getName() + "(";
    if (node.getParameters().size() > 0) {
      for (const auto &parameter : node.getParameters()) {
        result += typeToString(*parameter.type) + " " + parameter.name + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += ") -> " + typeToString(*node.getReturnType());
    return result;
  }
  std::string visitTypeAlias(const TypeAliasNode &node) override {
    return "type " + node.getName() + " = " + typeToString(*node.getType());
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

class TypeCheckTransformer : public AstToAstTransformVisitor {
public:
  std::unique_ptr<Type> visitExplicitType(const Type &type,
                                          const AstMetadata &metadata) {
    if (type.getTypeClass() == TypeClass::IDENTIFIED) {
      for (auto iterator = symbolTable.rbegin(); iterator != symbolTable.rend();
           iterator++) {
        auto symbols = *iterator;
        auto symbol = symbols.find(type.getName());
        if (symbol != symbols.end()) {
          const auto &symbolValue = symbol->second;
          if (symbolValue.isAstNode()) {
            const auto &symbolNode = symbolValue.getAstNode();
            if (symbolNode.getNodeType() == AstNodeType::TYPE_ALIAS) {
              const auto &typeAlias =
                  static_cast<const TypeAliasNode &>(symbolNode);
              return cloneType(*typeAlias.getType());
            }
          }
          errors.push_back(
              PyriteError{type.getName() + " is not a type", metadata});
        }
      }
      errors.push_back(
          PyriteError{type.getName() + " is not defined", metadata});
      return std::make_unique<VoidType>();
    } else {
      return cloneType(type);
    }
  }

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
    auto type = visitExplicitType(*node.getType(), node.getMetadata());
    auto initializer = visit(*node.getInitializer());
    const auto &initializerType = initializer->getValueType();
    convertTypesForAssignment(initializer, *type, initializerType);
    auto valueType = getVariableDefinitionValueType(
        cloneType(*type), !node.getMutable(), node.getMetadata());
    auto result = std::make_unique<VariableDefinitionNode>(
        std::move(type), node.getName(), std::move(initializer),
        node.getMutable(), modifyMetadata(node, std::move(valueType)));
    defineVariable(node.getName(), *result);
    return result;
  }
  ValueType
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    symbolTable.push_back({});
    auto returnType =
        visitExplicitType(*node.getReturnType(), node.getMetadata());
    std::vector<std::unique_ptr<Type>> parameterTypes;
    std::vector<NameAndType> newParameters;
    std::vector<std::unique_ptr<VariableDefinitionNode>>
        parameterVariables; // Houses the values in the symbol table.
    parameterVariables.reserve(node.getParameters().size());
    for (auto &parameter : node.getParameters()) {
      newParameters.push_back(
          NameAndType{parameter.name,
                      visitExplicitType(*parameter.type, node.getMetadata())});
      parameterTypes.push_back(
          visitExplicitType(*parameter.type, node.getMetadata()));
      AstMetadata parameterVariableMetadata;
      parameterVariableMetadata.valueType = getVariableDefinitionValueType(
          visitExplicitType(*parameter.type, node.getMetadata()), true,
          parameterVariableMetadata);
      parameterVariables.push_back(std::make_unique<VariableDefinitionNode>(
          visitExplicitType(*parameter.type, node.getMetadata()),
          parameter.name, std::unique_ptr<AstNode>(nullptr), false,
          std::move(parameterVariableMetadata)));
      defineVariable(parameter.name, *parameterVariables.back());
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
    parentFunctions.pop_back();
    auto result = std::make_unique<FunctionDefinitionNode>(
        node.getName(), std::move(newParameters),
        visitExplicitType(*node.getReturnType(), node.getMetadata()),
        std::move(body), node.getAttributes(),
        modifyMetadata(
            node, std::make_unique<ReferenceType>(
                      std::make_unique<FunctionType>(std::move(returnType),
                                                     std::move(parameterTypes)),
                      true)));
    symbolTable.pop_back();
    if (parentFunctions.size() > 0) {
      errors.push_back(PyriteError("Nested functions are not currently allowed",
                                   node.getMetadata()));
    } else {
      defineFunction(node.getName(), *result);
      result->setName(mangle(
          *result)); // Lets the rest of the processing forget about mangling.
    }
    return result;
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
    std::string translatedEscapeValue;
    try {
      translatedEscapeValue = translateEscapes(node.getValue());
    } catch (const std::runtime_error &e) {
      // It throws runtime errors because it doesn't have the information to
      // compose a PyriteError
      errors.push_back(PyriteError(e.what(), node.getMetadata()));
      translatedEscapeValue = node.getValue();
    }
    return std::make_unique<StringLiteralNode>(
        translatedEscapeValue,
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
                                    (*newValue)->getValueType());
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
    if (condition->getValueType().getTypeClass() != TypeClass::BOOLEAN) {
      errors.push_back(
          PyriteError("If condition must be a boolean", node.getMetadata()));
    }
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
    if (condition->getValueType().getTypeClass() != TypeClass::BOOLEAN) {
      errors.push_back(
          PyriteError("If condition must be a boolean", node.getMetadata()));
    }
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
    convertTypesForBinaryOperator(lhs, rhs, lhs->getValueType(),
                                  rhs->getValueType(), node.getMetadata());
    auto valueType = isComparisonOperator(node.getOp())
                         ? std::make_unique<BooleanType>()
                         : cloneType(lhs->getValueType());
    return std::make_unique<BinaryExpressionNode>(
        node.getOp(), std::move(lhs), std::move(rhs),
        modifyMetadata(node, std::move(valueType)));
  }
  ValueType visitUnaryExpression(const UnaryExpressionNode &node) override {
    auto operand = visit(*node.getOperand());
    convertTypesForUnaryOperator(operand, operand->getValueType(), node);
    return std::make_unique<UnaryExpressionNode>(
        node.getOp(), std::move(operand),
        modifyMetadata(node, cloneType(operand->getValueType())));
  }
  ValueType visitVariableReference(const VariableReferenceNode &node) override {
    const auto &name = node.getName();
    // Go through the symbol table backwards to get the most recent definition.
    for (auto symbolTableLevel = symbolTable.rbegin();
         symbolTableLevel != symbolTable.rend(); ++symbolTableLevel) {
      auto symbolTableEntry = symbolTableLevel->find(name);
      if (symbolTableEntry != symbolTableLevel->end()) {
        const auto &symbol = symbolTableEntry->second;
        if (symbol.isAstNode()) {
          const auto &definition = symbol.getAstNode();
          if (definition.getNodeType() == AstNodeType::TYPE_ALIAS) {
            errors.push_back(
                PyriteError(name + " is not a value", node.getMetadata()));
          }
          return std::make_unique<VariableReferenceNode>(
              name, modifyMetadata(node, cloneType(definition.getValueType())));
        } else {
          const auto &overloads = symbol.getOverloadList().functions;
          if (overloads.size() == 1) {
            return std::make_unique<VariableReferenceNode>(
                overloads.front()->getName(),
                modifyMetadata(node,
                               cloneType(overloads.front()->getValueType())));
          } else {
            std::vector<std::unique_ptr<FunctionType>> overloadTypes;
            overloadTypes.reserve(overloads.size());
            for (const auto &overload : overloads) {
              overloadTypes.push_back(staticCast<FunctionType>(
                  cloneType(removeReference(overload->getValueType()))));
            }
            return std::make_unique<VariableReferenceNode>(
                name,
                modifyMetadata(node, std::make_unique<OverloadedFunctionType>(
                                         std::move(overloadTypes))));
          }
        }
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
    if (newFunction->getValueType().getTypeClass() ==
        TypeClass::OVERLOADED_FUNCTION) {
      if (newFunction->getNodeType() != AstNodeType::VARIABLE_REFERENCE) {
        errors.push_back(PyriteError(
            "Overloaded function must be called directly", node.getMetadata()));
      } else {
        const auto &overloads = static_cast<const OverloadedFunctionType &>(
                                    newFunction->getValueType())
                                    .getOptions();
        std::vector<std::unique_ptr<FunctionType>> matchingOverloads;
        std::vector<std::vector<std::unique_ptr<AstNode>>> matchingArguments;
        for (const auto &overload : overloads) {
          if (overload->getParameters().size() == newArguments.size()) {
            bool matches = true;
            std::vector<std::unique_ptr<AstNode>> convertedArguments;
            for (size_t i = 0; i < newArguments.size(); ++i) {
              std::unique_ptr<AstNode> convertedArgument =
                  cloneAst(*newArguments[i]);
              if (!convertTypesForAssignment(
                      convertedArgument, *overload->getParameters()[i],
                      newArguments[i]->getValueType(), false)) {
                matches = false;
                break;
              } else {
                convertedArguments.push_back(std::move(convertedArgument));
              }
            }
            if (matches) {
              matchingOverloads.push_back(
                  staticCast<FunctionType>(cloneType(*overload)));
              matchingArguments.push_back(std::move(convertedArguments));
            }
          }
        }
        if (matchingOverloads.size() == 0) {
          errors.push_back(
              PyriteError{"No matching overload found", node.getMetadata()});
        } else if (matchingOverloads.size() > 1) {
          errors.push_back(
              PyriteError{"Ambiguous function call", node.getMetadata()});
        } else {
          const auto &variableReference =
              static_cast<const VariableReferenceNode &>(*newFunction);
          newArguments = std::move(matchingArguments.front());
          std::string mangledName =
              mangle(variableReference.getName(), *matchingOverloads.front());
          newFunction = visitVariableReference(
              VariableReferenceNode{mangledName, cloneMetadata(*newFunction)});
        }
      }
    }
    const auto &functionValueType = newFunction->getValueType();
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
      for (size_t i = 0; i < newArguments.size(); i++) {
        convertTypesForAssignment(newArguments[i],
                                  *functionType.getParameters()[i],
                                  newArguments[i]->getValueType());
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
    convertTypesForAssignment(newRhs, removeReference(newLhs->getValueType()),
                              newRhs->getValueType());
    auto valueType = cloneType(newLhs->getValueType());
    if (isConstantReference(newLhs->getValueType())) {
      errors.push_back(
          PyriteError("Cannot assign to a constant", node.getMetadata()));
    }
    return std::make_unique<AssignmentNode>(
        std::move(newLhs), std::move(newRhs), node.getAdditionalOperator(),
        modifyMetadata(node, std::move(valueType)));
  }
  ValueType visitDereference(const DereferenceNode &node) override {
    auto newReference = visit(*node.getValue());
    const auto &referenceValueType = newReference->getValueType();
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
    const auto &originalValueType = newValue->getValueType();
    auto newType = visitExplicitType(*node.getType(), node.getMetadata());
    // TODO: Check that the cast is valid.
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
  ValueType visitRawArrayLiteral(const RawArrayLiteralNode &) override {
    throw std::runtime_error(
        "Raw array literals should not be in this stage of the AST");
  }
  ValueType visitStructLiteral(const StructLiteralNode &node) override {
    std::vector<std::pair<std::string, ValueType>> newValues;
    for (auto &[name, value] : node.getValues()) {
      newValues.push_back({name, visit(*value)});
    }
    // Create a dummy struct type with what we have right now. The type
    // converter will pick up that we are a struct literal and fix it.
    std::vector<NameAndType> fields;
    for (auto &[name, value] : newValues) {
      fields.push_back({name, cloneType(value->getValueType())});
    }
    return std::make_unique<StructLiteralNode>(
        std::move(newValues),
        modifyMetadata(node, std::make_unique<StructType>(std::move(fields))));
  }
  ValueType visitArrayIndex(const ArrayIndexNode &node) override {
    auto newArray = visit(*node.getArray());
    auto newIndex = visit(*node.getIndex());
    const auto &arrayValueType = newArray->getValueType();
    if (arrayValueType.getTypeClass() != TypeClass::REFERENCE) {
      errors.push_back(
          PyriteError("Cannot index a non-array reference of type " +
                          typeToString(arrayValueType),
                      node.getMetadata()));
      return std::make_unique<ArrayIndexNode>(
          std::move(newArray), std::move(newIndex),
          modifyMetadata(node, cloneType(arrayValueType)));
    }
    const auto &arrayReferenceType =
        static_cast<const ReferenceType &>(arrayValueType);
    const auto &dereferencedArrayType = removeReference(arrayValueType);
    if (dereferencedArrayType.getTypeClass() != TypeClass::ARRAY) {
      errors.push_back(PyriteError("Cannot index a non-array of type " +
                                       typeToString(dereferencedArrayType),
                                   node.getMetadata()));
      return std::make_unique<ArrayIndexNode>(
          std::move(newArray), std::move(newIndex),
          modifyMetadata(node, cloneType(arrayValueType)));
    }
    const auto &arrayType =
        static_cast<const ArrayType &>(dereferencedArrayType);
    convertTypesForAssignment(newIndex, IntegerType{64, false},
                              newIndex->getValueType());
    auto newValueType =
        std::make_unique<ReferenceType>(cloneType(arrayType.getElementType()),
                                        arrayReferenceType.getConstant());
    return std::make_unique<ArrayIndexNode>(
        std::move(newArray), std::move(newIndex),
        modifyMetadata(node, std::move(newValueType)));
  }
  ValueType visitRawArrayIndex(const RawArrayIndexNode &) override {
    throw std::runtime_error(
        "Raw array index should not be in this stage of the AST");
  }
  ValueType visitStructMember(const StructMemberNode &node) override {
    auto newStruct = visit(*node.getStructValue());
    const auto &structValueType = newStruct->getValueType();
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
    auto memberReferenceType = std::make_unique<ReferenceType>(
        cloneType(**memberType), structReferenceType.getConstant());
    return std::make_unique<StructMemberNode>(
        std::move(newStruct), node.getMember(),
        modifyMetadata(node, std::move(memberReferenceType)));
  }
  ValueType visitAssert(const AssertNode &) override {
    throw std::runtime_error("Assert should not be in this stage of the AST");
  }
  ValueType visitExternalFunction(const ExternalFunctionNode &node) override {
    std::vector<NameAndType> newParameters;
    for (const auto &parameter : node.getParameters()) {
      newParameters.push_back(
          {parameter.name,
           visitExplicitType(*parameter.type, node.getMetadata())});
    }
    std::vector<std::unique_ptr<Type>> parameterTypes;
    for (const auto &parameter : newParameters) {
      parameterTypes.push_back(
          visitExplicitType(*parameter.type, node.getMetadata()));
    }
    auto functionType = std::make_unique<FunctionType>(
        visitExplicitType(*node.getReturnType(), node.getMetadata()),
        std::move(parameterTypes));
    auto valueType =
        std::make_unique<ReferenceType>(std::move(functionType), true);
    auto result = std::make_unique<ExternalFunctionNode>(
        node.getName(), std::move(newParameters),
        visitExplicitType(*node.getReturnType(), node.getMetadata()),
        modifyMetadata(node, std::move(valueType)));
    defineVariable(node.getName(), *result);
    return result;
  }
  ValueType visitTypeAlias(const TypeAliasNode &node) override {
    auto newType = visitExplicitType(*node.getType(), node.getMetadata());
    newType->setName(node.getName());
    auto result = std::make_unique<TypeAliasNode>(
        node.getName(), std::move(newType),
        modifyMetadata(node, std::make_unique<VoidType>()));
    defineVariable(node.getName(), *result);
    return result;
  }

private:
  struct OverloadList {
    std::vector<const FunctionDefinitionNode *> functions;
  };
  struct SymbolTableEntry {
    std::variant<const AstNode *, OverloadList> value;

    SymbolTableEntry(const AstNode &astNode) : value(&astNode) {}
    SymbolTableEntry(OverloadList overloadList)
        : value(std::move(overloadList)) {}

    bool isAstNode() const {
      return std::holds_alternative<const AstNode *>(value);
    }
    const AstNode &getAstNode() const {
      return *std::get<const AstNode *>(value);
    }

    bool isOverloadList() const {
      return std::holds_alternative<OverloadList>(value);
    }
    const OverloadList &getOverloadList() const {
      return std::get<OverloadList>(value);
    }
    OverloadList &getOverloadList() { return std::get<OverloadList>(value); }
  };

  std::vector<std::map<std::string, SymbolTableEntry>> symbolTable;
  std::vector<const FunctionDefinitionNode *> parentFunctions;

  void defineVariable(std::string name, const AstNode &astNode) {
    if (symbolTable.back().contains(name)) {
      errors.push_back(
          PyriteError(name + " is already defined", astNode.getMetadata()));
    }
    symbolTable.back().insert({std::move(name), astNode});
  }
  void defineFunction(std::string name,
                      const FunctionDefinitionNode &definition) {
    // C-exported functions are not allowed to be overloaded
    if (definition.getCExported()) {
      defineVariable(std::move(name), definition); // Same logic applies
    } else {
      // There should always be an entry for the mangled name to allow for
      // lookup by variable references which are re-visited by the overload
      // resolution process.
      defineVariable(mangle(definition), definition);
      if (symbolTable.back().contains(name)) {
        auto &entry = symbolTable.back().at(name);
        if (entry.isAstNode()) {
          errors.push_back(PyriteError(name + " is already defined",
                                       entry.getAstNode().getMetadata()));
        } else {
          entry.getOverloadList().functions.push_back(&definition);
        }
      } else {
        symbolTable.back().insert(
            {std::move(name), OverloadList{{&definition}}});
      }
    }
  }
};

std::unique_ptr<AstNode> typeCheck(const AstNode &ast) {
  TypeCheckTransformer transformer;
  return transformer.visit(ast);
}

class MoveAnalyzer : public PartialAstToAstTransformerVisitor {
public:
  ValueType visitCompilationUnit(const CompilationUnitNode &node) override {
    createScope();
    auto result = PartialAstToAstTransformerVisitor::visitCompilationUnit(node);
    destroyScope();
    return result;
  }
  ValueType visitBlockStatement(const BlockStatementNode &node) override {
    createScope();
    auto result = PartialAstToAstTransformerVisitor::visitBlockStatement(node);
    destroyScope();
    return result;
  }

  ValueType visitDereference(const DereferenceNode &node) override {
    ValueType result =
        PartialAstToAstTransformerVisitor::visitDereference(node);
    if (!isCopyable(node.getValueType())) {
      // This is where movement happens.
      const auto &value = *node.getValue();
      AstNodeType valueNodeType = value.getNodeType();
      switch (valueNodeType) {
      case AstNodeType::VARIABLE_REFERENCE: {
        const auto &variableReferenceNode =
            static_cast<const VariableReferenceNode &>(value);
        moveVariable(variableReferenceNode.getName());
        break;
      }
      default:
        errors.push_back(PyriteError{"Can't move out of this expression",
                                     node.getMetadata()});
      }
    }
    return result;
  }

  ValueType visitVariableReference(const VariableReferenceNode &node) override {
    if (isMoved(node.getName())) {
      errors.push_back(
          PyriteError{node.getName() + " is moved", node.getMetadata()});
    }
    return PartialAstToAstTransformerVisitor::visitVariableReference(node);
  }

  ValueType
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    allVariables.back().insert(node.getName());
    return PartialAstToAstTransformerVisitor::visitVariableDefinition(node);
  }

private:
  std::vector<std::set<std::string>> movedVariables;
  std::vector<std::set<std::string>> allVariables;

  void createScope() {
    movedVariables.push_back({});
    allVariables.push_back({});
  }
  void destroyScope() {
    movedVariables.pop_back();
    allVariables.pop_back();
  }

  void moveVariable(const std::string &name) {
    // We should add it to the right level of the table so that it remains
    // "moved" until the end of its lifetime.
    for (size_t i = allVariables.size(); i-- > 0;) {
      if (allVariables[i].contains(name)) {
        movedVariables[i].insert(name);
        break;
      }
    }
  }

  bool isMoved(const std::string &name) {
    // Go back through the "allVariables" list to ensure we don't give false
    // positives from a moved variable which is obscured by a more recent
    // definition.
    for (size_t i = allVariables.size(); i-- > 0;) {
      if (allVariables[i].contains(name)) {
        return movedVariables[i].contains(name);
      }
    }
    // Bearing in mind that functions aren't in the allVariables table, we
    // should just return false here.
    return false;
  }

  bool isCopyable(const Type &type) {
    auto typeClass = type.getTypeClass();
    switch (typeClass) {
    case TypeClass::INTEGER:
    case TypeClass::FLOAT:
    case TypeClass::BOOLEAN:
    case TypeClass::CHAR:
    case TypeClass::ENUM:
      return true;
    default:
      return false;
    }
  }
};

std::unique_ptr<AstNode> analyseMoves(const AstNode &ast) {
  MoveAnalyzer moveAnalyzer;
  return moveAnalyzer.visit(ast);
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
    if (value->hasValueType()) {
      value->setValueType(visitType(value->getValueType()));
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
        visit(*node.getBody()), node.getAttributes(),
        node.getMetadata().clone());
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
      const auto &expressionType = node.getValue()->getValueType();
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

class ComplexEntityToStructTransformer
    : public PartialAstToAstTransformerVisitor {
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

  class ComplexEntityToStructTypeTransformer
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
      return std::make_unique<StructType>(std::move(memberTypes));
    }
    ValueType visitArray(const ArrayType &type) override {
      auto rawArrayType =
          std::make_unique<RawArrayType>(visit(type.getElementType()));
      std::vector<NameAndType> memberTypes;
      memberTypes.emplace_back("size",
                               std::make_unique<IntegerType>(64, false));
      memberTypes.emplace_back("data", std::move(rawArrayType));
      return std::make_unique<StructType>(std::move(memberTypes));
    }
  };

public:
  ComplexEntityToStructTransformer(const TypeIdCollector &typeIdCollector)
      : typeIdCollector(typeIdCollector) {}

  ValueType visit(const AstNode &ast) {
    auto result = PartialAstToAstTransformerVisitor::visit(ast);
    AstTypeTransformer typeTransformer{ComplexEntityToStructTypeTransformer{}};
    result = typeTransformer.visit(*result);
    return result;
  }

  ValueType visitCast(const CastNode &node) override {
    if (node.getType()->getTypeClass() == TypeClass::UNION) {
      const auto &unionType = static_cast<const UnionType &>(*node.getType());
      const auto &expressionType = node.getValue()->getValueType();
      if (expressionType.getTypeClass() != TypeClass::UNION) {
        std::optional<ValueType> descriminator = std::nullopt;
        ValueType value = visit(*node.getValue());
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
        std::vector<std::pair<std::string, ValueType>> fields;
        fields.emplace_back(UNION_DISCRIMINATOR_FIELD,
                            std::move(*descriminator));
        fields.emplace_back(UNION_VALUE_FIELD, std::move(value));
        return std::make_unique<StructLiteralNode>(std::move(fields),
                                                   node.getMetadata().clone());
      }
    }
    return PartialAstToAstTransformerVisitor::visitCast(node);
  }

  ValueType visitArrayLiteral(const ArrayLiteralNode &node) override {
    const auto &arrayType = static_cast<const ArrayType &>(node.getValueType());
    std::vector<ValueType> values;
    for (const auto &value : node.getValues()) {
      values.push_back(visit(*value));
    }
    AstMetadata dataMetadata = node.getMetadata().clone();
    dataMetadata.valueType =
        std::make_unique<RawArrayType>(cloneType(arrayType.getElementType()));
    std::vector<std::pair<std::string, ValueType>> fields;
    auto sizeMetadata = dataMetadata.clone();
    sizeMetadata.valueType = std::make_unique<IntegerType>(64, false);
    fields.emplace_back("size", std::make_unique<IntegerLiteralNode>(
                                    values.size(), std::move(sizeMetadata)));
    fields.emplace_back(
        "data", std::make_unique<RawArrayLiteralNode>(std::move(values),
                                                      std::move(dataMetadata)));
    return std::make_unique<StructLiteralNode>(std::move(fields),
                                               node.getMetadata().clone());
  }

  ValueType visitArrayIndex(const ArrayIndexNode &node) override {
    ValueType array = visit(*node.getArray());
    ValueType index = visit(*node.getIndex());
    // We need to access the array twice (once for the data and once for the
    // size) so we have to put it into a temporary variable. Get the data member
    auto arrayTemporary = createTemporaryVariable(std::move(array));
    // Get the size member and assert that index is less than it.
    // We do this first so we can use the arrayTemporary before it is moved for
    // the data member logic.
    auto sizeMetadata = arrayTemporary->getMetadata().clone();
    sizeMetadata.valueType = std::make_unique<IntegerType>(64, false);
    auto sizeMemberPointer = std::make_unique<StructMemberNode>(
        createTemporaryVariableReference(*arrayTemporary), "size",
        std::move(sizeMetadata));
    AstMetadata sizeMemberMetadata = sizeMemberPointer->getMetadata().clone();
    sizeMemberMetadata.valueType =
        cloneType(removeReference(**sizeMemberMetadata.valueType));
    auto sizeMember = std::make_unique<DereferenceNode>(
        std::move(sizeMemberPointer), std::move(sizeMemberMetadata));
    auto dataMetadata = arrayTemporary->getMetadata().clone();
    dataMetadata.valueType = std::make_unique<RawArrayType>(
        cloneType(removeReference(node.getValueType())));
    auto dataMemberPointer = std::make_unique<StructMemberNode>(
        std::move(arrayTemporary), "data",
        std::move(dataMetadata)); // arrayTemporary is a reference, which is
                                  // what we want just now.
    AstMetadata dataMemberMetadata = dataMemberPointer->getMetadata().clone();
    dataMemberMetadata.valueType =
        cloneType(removeReference(**dataMemberMetadata.valueType));
    auto dataMember = std::make_unique<DereferenceNode>(
        std::move(dataMemberPointer), std::move(dataMemberMetadata));
    auto indexAssertMetadata = node.getIndex()->getMetadata().clone();
    // TODO: use a proper handling system for panic.
    auto panicMetadata = indexAssertMetadata.clone(); // "u64"
    index = std::make_unique<AssertNode>(
        std::move(index), std::move(sizeMember), true,
        BinaryOperator::LESS_THAN,
        std::make_unique<IntegerLiteralNode>(123, std::move(panicMetadata)),
        std::move(indexAssertMetadata));
    return std::make_unique<RawArrayIndexNode>(
        std::move(dataMember), std::move(index), node.getMetadata().clone());
  }

  ValueType visitStringLiteral(const StringLiteralNode &node) override {
    std::vector<ValueType> characterValues;
    AstMetadata characterValueMetadata = node.getMetadata().clone();
    characterValueMetadata.valueType = std::make_unique<IntegerType>(8, true);
    for (char c : node.getValue()) {
      characterValues.push_back(std::make_unique<IntegerLiteralNode>(
          c, characterValueMetadata.clone()));
    }
    // To simplify the process we can just call visitArrayLiteral on a made-up
    // array literal. This will avoid duplication and make it real easy.
    return visit(ArrayLiteralNode{std::move(characterValues),
                                  node.getMetadata().clone()});
  }

private:
  const TypeIdCollector &typeIdCollector;
};

std::unique_ptr<AstNode> simplifyAst(const AstNode &ast) {
  TypeIdCollector typeIdCollector;
  ast.accept(typeIdCollector);
  AstTypeTransformer<AnyToUnionTypeTransformer> anyToUnionTransformer{
      AnyToUnionTypeTransformer{typeIdCollector}};
  ComplexEntityToStructTransformer complexEntityToStructTransformer{
      typeIdCollector};
  auto result = anyToUnionTransformer.visit(ast);
  result = complexEntityToStructTransformer.visit(*result);
  return result;
}
} // namespace pyrite
