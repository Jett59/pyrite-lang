#include "type.h"
#include "ast.h"
#include "error.h"

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
        result += visit(*param);
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

static PyriteException typeMismatch(const Type &expected, const Type &actual,
                                    const AstMetadata &astMetadata) {
  return PyriteException("Type mismatch: expected " + typeToString(expected) +
                             ", got " + typeToString(actual),
                         astMetadata);
}

static bool emitCast(const Type &from, const Type &to,
                     std::unique_ptr<AstNode> &astNode) {
  bool canCast = false;
  if (to.getTypeClass() == TypeClass::ANY) {
    canCast = true;
  } else if (to.getTypeClass() == TypeClass::UNION) {
    const UnionType &unionToType = static_cast<const UnionType &>(to);
    for (const auto &option : unionToType.getOptions()) {
      if (emitCast(from, *option, astNode)) {
        canCast = true;
        break;
      }
    }
  } else if (to.getTypeClass() == TypeClass::INTEGER &&
             from.getTypeClass() == TypeClass::INTEGER) {
    const IntegerType &integerToType = static_cast<const IntegerType &>(to);
    const IntegerType &integerFromType = static_cast<const IntegerType &>(from);
    if (integerToType.getBits() >= integerFromType.getBits()) {
      canCast = true;
    }
  } else if (to.getTypeClass() == TypeClass::FLOAT &&
             from.getTypeClass() == TypeClass::FLOAT) {
    const FloatType &floatToType = static_cast<const FloatType &>(to);
    const FloatType &floatFromType = static_cast<const FloatType &>(from);
    if (floatToType.getBits() >= floatFromType.getBits()) {
      canCast = true;
    }
  }
  if (canCast) {
    AstMetadata newMetadata = astNode->getMetadata().clone();
    newMetadata.valueType = cloneType(to);
    astNode = std::make_unique<CastNode>(std::move(astNode), cloneType(to),
                                         std::move(newMetadata));
  }
  return canCast;
}

void convertTypesForAssignment(std::unique_ptr<AstNode> &rhsAstNode,
                               const Type &lhs, const Type &rhs) {
  auto lhsType = &lhs;
  auto rhsType = &rhs;
  if (lhsType->getTypeClass() != TypeClass::REFERENCE &&
      rhsType->getTypeClass() == TypeClass::REFERENCE) {
    AstMetadata newMetadata = rhsAstNode->getMetadata().clone();
    newMetadata.valueType = cloneType(
        static_cast<const ReferenceType &>(*rhsType).getReferencedType());
    rhsAstNode = std::make_unique<DereferenceNode>(std::move(rhsAstNode),
                                                   std::move(newMetadata));
    rhsType = &**rhsAstNode->getMetadata().valueType;
  }
  TypeClass lhsTypeClass = lhsType->getTypeClass();
  TypeClass rhsTypeClass = rhsType->getTypeClass();
  if (lhsTypeClass == TypeClass::REFERENCE &&
      rhsTypeClass == TypeClass::REFERENCE) {
    const auto &lhsReferenceType = static_cast<const ReferenceType &>(*lhsType);
    const auto &rhsReferenceType = static_cast<const ReferenceType &>(*rhsType);
    if (rhsReferenceType.getConstant() && !lhsReferenceType.getConstant()) {
      throw PyriteException(
          "Cannot assign constant reference to mutable reference",
          rhsAstNode->getMetadata());
    }
  } else if (lhsTypeClass == TypeClass::ANY && rhsTypeClass != TypeClass::ANY) {
    AstMetadata newMetadata = rhsAstNode->getMetadata().clone();
    newMetadata.valueType = cloneType(*lhsType);
    rhsAstNode = std::make_unique<CastNode>(
        std::move(rhsAstNode), cloneType(*lhsType), std::move(newMetadata));
    rhsType = &**rhsAstNode->getMetadata().valueType;
    rhsTypeClass = rhsType->getTypeClass();
  } else if (lhsTypeClass == TypeClass::UNION) {
  }
}
} // namespace pyrite
