#include "type.h"
#include "ast.h"
#include "error.h"
#include <algorithm>

namespace pyrite {
std::unique_ptr<Type> cloneType(const Type &type) {
  // The PartialTypeToTypeTransformVisitor copies all elements by default, which
  // means it is perfectly suited to this task.
  PartialTypeToTypeTransformVisitor transformer;
  return transformer.visit(type);
}

class TypeToStringTransformer : public TypeTransformVisitor<std::string> {
public:
  std::string visitVoid(const VoidType &type) override { return "void"; }
  std::string visitInteger(const IntegerType &type) override {
    return (type.getSigned() ? "i" : "u") + std::to_string(type.getBits());
  }
  std::string visitFloat(const FloatType &type) override {
    return "f" + std::to_string(type.getBits());
  }
  std::string visitBoolean(const BooleanType &type) override { return "bool"; }
  std::string visitChar(const CharType &type) override { return "char"; }
  std::string visitArray(const ArrayType &type) override {
    return "[" + visit(type.getElementType()) + "]";
  }
  std::string visitReference(const ReferenceType &type) override {
    return std::string("&") + (type.getConstant() ? "" : "mut ") +
           visit(type.getReferencedType());
  }
  std::string visitFunction(const FunctionType &type) override {
    std::string result = visit(type.getReturnType()) + "(";
    if (type.getParameters().size() > 0) {
      for (const auto &param : type.getParameters()) {
        result += visit(*param) + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += ")";
    return result;
  }
  std::string visitStruct(const StructType &type) override {
    std::string result = "{";
    if (type.getFields().size() > 0) {
      for (const auto &field : type.getFields()) {
        result += visit(*field.type) + " " + field.name + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += "}";
    return result;
  }
  std::string visitUnion(const UnionType &type) override {
    std::string result = "any(";
    if (type.getOptions().size() > 0) {
      for (const auto &option : type.getOptions()) {
        result += visit(*option) + "|";
      }
      result = result.substr(0, result.size() - 1);
    }
    result += ")";
    return result;
  }
  std::string visitRawUnion(const RawUnionType &type) override {
    std::string result = "union(";
    if (type.getOptions().size() > 0) {
      for (const auto &option : type.getOptions()) {
        result += visit(*option) + "|";
      }
      result = result.substr(0, result.size() - 1);
    }
    result += ")";
    return result;
  }
  std::string visitEnum(const EnumType &type) override {
    std::string result = "enum(";
    if (type.getOptions().size() > 0) {
      for (const auto &option : type.getOptions()) {
        result += option + "|";
      }
      result = result.substr(0, result.size() - 1);
    }
    result += ")";
    return result;
  }
  std::string visitIdentified(const IdentifiedType &type) override {
    return type.getName();
  }
  std::string visitAny(const AnyType &type) override { return "any"; }
  std::string visitAuto(const AutoType &type) override { return "auto"; }
};

std::string typeToString(const Type &type) {
  TypeToStringTransformer transformer;
  return transformer.visit(type);
}

class TypesEqualTransformer : public TypeTransformVisitor<bool> {
public:
  TypesEqualTransformer(const Type &other) : other(other) {}

  bool visitVoid(const VoidType &type) override {
    return other.getTypeClass() == TypeClass::VOID;
  }
  bool visitInteger(const IntegerType &type) override {
    if (other.getTypeClass() != TypeClass::INTEGER) {
      return false;
    } else {
      const IntegerType &otherInteger = static_cast<const IntegerType &>(other);
      return type.getBits() == otherInteger.getBits() &&
             type.getSigned() == otherInteger.getSigned();
    }
  }
  bool visitFloat(const FloatType &type) override {
    if (other.getTypeClass() != TypeClass::FLOAT) {
      return false;
    } else {
      const FloatType &otherFloat = static_cast<const FloatType &>(other);
      return type.getBits() == otherFloat.getBits();
    }
  }
  bool visitBoolean(const BooleanType &type) override {
    return other.getTypeClass() == TypeClass::BOOLEAN;
  }
  bool visitChar(const CharType &type) override {
    return other.getTypeClass() == TypeClass::CHAR;
  }
  bool visitArray(const ArrayType &type) override {
    if (other.getTypeClass() != TypeClass::ARRAY) {
      return false;
    } else {
      const ArrayType &otherArray = static_cast<const ArrayType &>(other);
      return TypesEqualTransformer{otherArray.getElementType()}.visit(
          type.getElementType());
    }
  }
  bool visitReference(const ReferenceType &type) override {
    if (other.getTypeClass() != TypeClass::REFERENCE) {
      return false;
    } else {
      const ReferenceType &otherReference =
          static_cast<const ReferenceType &>(other);
      return type.getConstant() == otherReference.getConstant() &&
             TypesEqualTransformer{otherReference.getReferencedType()}.visit(
                 type.getReferencedType());
    }
  }
  bool visitFunction(const FunctionType &type) override {
    if (other.getTypeClass() != TypeClass::FUNCTION) {
      return false;
    } else {
      const FunctionType &otherFunction =
          static_cast<const FunctionType &>(other);
      if (type.getParameters().size() != otherFunction.getParameters().size()) {
        return false;
      } else {
        for (size_t i = 0; i < type.getParameters().size(); i++) {
          if (!TypesEqualTransformer{*otherFunction.getParameters()[i]}.visit(
                  *type.getParameters()[i])) {
            return false;
          }
        }
        return TypesEqualTransformer{otherFunction.getReturnType()}.visit(
            type.getReturnType());
      }
    }
  }
  bool visitStruct(const StructType &type) override {
    if (other.getTypeClass() != TypeClass::STRUCT) {
      return false;
    } else {
      const StructType &otherStruct = static_cast<const StructType &>(other);
      if (type.getFields().size() != otherStruct.getFields().size()) {
        return false;
      } else {
        for (size_t i = 0; i < otherStruct.getFields().size(); i++) {
          if (!TypesEqualTransformer{*otherStruct.getFields()[i].type}.visit(
                  *type.getFields()[i].type)) {
            return false;
          }
        }
        return true;
      }
    }
  }
  bool visitUnion(const UnionType &type) override {
    if (other.getTypeClass() != TypeClass::UNION) {
      return false;
    } else {
      const UnionType &otherUnion = static_cast<const UnionType &>(other);
      if (type.getOptions().size() != otherUnion.getOptions().size()) {
        return false;
      } else {
        for (size_t i = 0; i < otherUnion.getOptions().size(); i++) {
          if (!TypesEqualTransformer{*otherUnion.getOptions()[i]}.visit(
                  *type.getOptions()[i])) {
            return false;
          }
        }
        return true;
      }
    }
  }
  bool visitRawUnion(const RawUnionType &type) override {
    if (other.getTypeClass() != TypeClass::RAW_UNION) {
      return false;
    } else {
      const auto &otherUnion = static_cast<const RawUnionType &>(other);
      if (type.getOptions().size() != otherUnion.getOptions().size()) {
        return false;
      } else {
        for (size_t i = 0; i < otherUnion.getOptions().size(); i++) {
          if (!TypesEqualTransformer{*otherUnion.getOptions()[i]}.visit(
                  *type.getOptions()[i])) {
            return false;
          }
        }
        return true;
      }
    }
  }
  bool visitEnum(const EnumType &type) override {
    if (other.getTypeClass() != TypeClass::ENUM) {
      return false;
    } else {
      const EnumType &otherEnum = static_cast<const EnumType &>(other);
      if (type.getOptions().size() != otherEnum.getOptions().size()) {
        return false;
      } else {
        for (size_t i = 0; i < type.getOptions().size(); i++) {
          if (type.getOptions()[i] != otherEnum.getOptions()[i]) {
            return false;
          }
        }
        return true;
      }
    }
  }
  bool visitIdentified(const IdentifiedType &type) override {
    if (other.getTypeClass() != TypeClass::IDENTIFIED) {
      return false;
    } else {
      const IdentifiedType &otherIdentified =
          static_cast<const IdentifiedType &>(other);
      return type.getName() == otherIdentified.getName();
    }
  }
  bool visitAny(const AnyType &type) override {
    return other.getTypeClass() == TypeClass::ANY;
  }
  bool visitAuto(const AutoType &type) override {
    return other.getTypeClass() == TypeClass::AUTO;
  }

private:
  const Type &other;
};
bool typeEquals(const Type &a, const Type &b) {
  return TypesEqualTransformer{a}.visit(b);
}

static PyriteError typeMismatch(const Type &expected, const Type &actual,
                                const AstMetadata &astMetadata) {
  return PyriteError("Type mismatch: expected " + typeToString(expected) +
                         ", got " + typeToString(actual),
                     astMetadata);
}
static PyriteError incompatibleTypes(const Type &a, const Type &b,
                                     const AstMetadata &metadata) {
  return PyriteError("Type mismatch: " + typeToString(a) +
                         " is not compatible with " + typeToString(b),
                     metadata);
}
static PyriteError convertionBetweenSigns(const Type &a, const Type &b,
                                          const AstMetadata &metadata) {
  return PyriteError("Cannot convert between " + typeToString(a) + " and " +
                         typeToString(b) + " of different sign",
                     metadata);
}
static PyriteError lossyConvertion(const Type &from, const Type &to,
                                   const AstMetadata &metadata) {
  return PyriteError("Converting from " + typeToString(from) + " to " +
                         typeToString(to) + " loses precision",
                     metadata);
}

static bool emitCast(const Type &from, const Type &to,
                     std::unique_ptr<AstNode> &astNode,
                     bool emitErrors = true) {
  bool canCast = false;
  bool typesEqual = false;
  bool hasErrors = false;
  if (typeEquals(to, from)) {
    canCast = true;
    typesEqual = true;
  } else if (to.getTypeClass() == TypeClass::ANY) {
    canCast = true;
  } else if (to.getTypeClass() == TypeClass::UNION) {
    const UnionType &unionToType = static_cast<const UnionType &>(to);
    for (const auto &option : unionToType.getOptions()) {
      if (emitCast(from, *option, astNode, false)) {
        canCast = true;
        break;
      }
    }
  } else if (to.getTypeClass() == TypeClass::INTEGER &&
             from.getTypeClass() == TypeClass::INTEGER) {
    const IntegerType &integerToType = static_cast<const IntegerType &>(to);
    if (astNode->getNodeType() == AstNodeType::INTEGER_LITERAL) {
      const IntegerLiteralNode &integerLiteral =
          static_cast<const IntegerLiteralNode &>(*astNode);
      size_t unsignedBits = integerToType.getBits() - integerToType.getSigned();
      uint64_t maxValue = (1 << unsignedBits) - 1;
      int64_t minValue = integerToType.getSigned() ? -maxValue - 1 : 0;
      int64_t value = integerLiteral.getValue();
      if (value >= minValue && static_cast<uint64_t>(value) <= maxValue) {
        // Dirty, but we can just set the type on the metadata here which avoids
        // excess copying.
        astNode->getMetadata().valueType = cloneType(integerToType);
        typesEqual = true;
      }
    }
    // typesEqual will be true if we can cast the integer literal.
    if (!typesEqual) {
      const IntegerType &integerFromType =
          static_cast<const IntegerType &>(from);
      if (integerToType.getBits() < integerFromType.getBits()) {
        hasErrors = true;
        if (emitErrors) {
          errors.push_back(lossyConvertion(from, to, astNode->getMetadata()));
        }
      }
      if (integerToType.getSigned() != integerFromType.getSigned()) {
        hasErrors = true;
        if (emitErrors) {
          errors.push_back(
              convertionBetweenSigns(from, to, astNode->getMetadata()));
        }
      }
      canCast = true;
    }
  } else if (to.getTypeClass() == TypeClass::FLOAT &&
             from.getTypeClass() == TypeClass::FLOAT) {
    const FloatType &floatToType = static_cast<const FloatType &>(to);
    const FloatType &floatFromType = static_cast<const FloatType &>(from);
    if (floatToType.getBits() < floatFromType.getBits()) {
      hasErrors = true;
      if (emitErrors) {
        errors.push_back(lossyConvertion(from, to, astNode->getMetadata()));
      }
    }
    canCast = true;
  }
  if (!typesEqual) {
    if (canCast) {
      AstMetadata newMetadata = astNode->getMetadata().clone();
      newMetadata.valueType = cloneType(to);
      astNode = std::make_unique<CastNode>(std::move(astNode), cloneType(to),
                                           std::move(newMetadata));
    } else {
      hasErrors = true;
      if (emitErrors) {
        errors.push_back(typeMismatch(to, from, astNode->getMetadata()));
      }
    }
  }
  return !hasErrors;
}

void removeReference(const Type &type, std::unique_ptr<AstNode> &astNode) {
  if (type.getTypeClass() == TypeClass::REFERENCE) {
    AstMetadata newMetadata = astNode->getMetadata().clone();
    newMetadata.valueType =
        cloneType(static_cast<const ReferenceType &>(type).getReferencedType());
    astNode = std::make_unique<DereferenceNode>(std::move(astNode),
                                                std::move(newMetadata));
  }
}

void convertTypesForAssignment(std::unique_ptr<AstNode> &rhsAstNode,
                               const Type &lhs, const Type &rhs) {
  auto lhsType = &lhs;
  auto rhsType = &rhs;
  if (lhsType->getTypeClass() != TypeClass::REFERENCE &&
      rhsType->getTypeClass() == TypeClass::REFERENCE) {
    removeReference(*rhsType, rhsAstNode);
    rhsType = &**rhsAstNode->getMetadata().valueType;
  }
  emitCast(*rhsType, *lhsType, rhsAstNode);
  rhsType = &**rhsAstNode->getMetadata().valueType;
}
void convertTypesForBinaryOperator(std::unique_ptr<AstNode> &lhsAstNode,
                                   std::unique_ptr<AstNode> &rhsAstNode,
                                   const Type &lhs, const Type &rhs,
                                   const AstMetadata &expressionMetadata) {
  // Binary expressions don't like references. If they did, then it would've
  // already been replaced with a function call to a user-defined operator.
  const Type *lhsType = &lhs;
  const Type *rhsType = &rhs;
  removeReference(*lhsType, lhsAstNode);
  removeReference(*rhsType, rhsAstNode);
  lhsType = &**lhsAstNode->getMetadata().valueType;
  rhsType = &**rhsAstNode->getMetadata().valueType;
  if (lhsType->getTypeClass() != rhsType->getTypeClass()) {
    errors.push_back(incompatibleTypes(*lhsType, *rhsType, expressionMetadata));
  }
  if (lhsType->getTypeClass() == TypeClass::INTEGER) {
    const auto &lhsInteger = static_cast<const IntegerType &>(*lhsType);
    const auto &rhsInteger = static_cast<const IntegerType &>(*rhsType);
    std::unique_ptr<Type> integerType = std::make_unique<IntegerType>(
        std::max(lhsInteger.getBits(), rhsInteger.getBits()),
        std::max(lhsInteger.getSigned(), rhsInteger.getSigned()));
    emitCast(*lhsType, *integerType, lhsAstNode);
    emitCast(*rhsType, *integerType, rhsAstNode);
  } else if (lhsType->getTypeClass() == TypeClass::FLOAT) {
    const auto &lhsFloat = static_cast<const FloatType &>(*lhsType);
    const auto &rhsFloat = static_cast<const FloatType &>(*rhsType);
    std::unique_ptr<Type> floatType = std::make_unique<FloatType>(
        std::max(lhsFloat.getBits(), rhsFloat.getBits()));
    emitCast(*lhsType, *floatType, lhsAstNode);
    emitCast(*rhsType, *floatType, rhsAstNode);
  } else {
    errors.push_back(PyriteError(
        "Invalid operands for binary operator: " + typeToString(*lhsType) +
            " and " + typeToString(*rhsType),
        expressionMetadata));
  }
}
void convertTypesForUnaryOperator(std::unique_ptr<AstNode> &valueAstNode,
                                  const Type &type,
                                  const UnaryExpressionNode &unaryExpression) {
  UnaryOperator op = unaryExpression.getOp();
  const Type *valueType = &type;
  removeReference(*valueType, valueAstNode);
  valueType = &**valueAstNode->getMetadata().valueType;
  if (valueType->getTypeClass() == TypeClass::INTEGER) {
    const auto &integer = static_cast<const IntegerType &>(*valueType);
    if (op == UnaryOperator::NEGATE && !integer.getSigned()) {
      errors.push_back(PyriteError("Can't negate unsigned integer",
                                   valueAstNode->getMetadata()));
    }
  } else if (valueType->getTypeClass() != TypeClass::FLOAT) {
    errors.push_back(PyriteError("Invalid operand for unary operator: " +
                                     typeToString(*valueType),
                                 valueAstNode->getMetadata()));
  }
}
} // namespace pyrite
