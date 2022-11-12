#include "type.h"

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
        result += visit(*field.second) + " " + field.first + ", ";
      }
      result = result.substr(0, result.size() - 2);
    }
    result += "}";
    return result;
  }
  std::string visitUnion(const UnionType &type) override {
    std::string result = "(";
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
        result += option.first + "|";
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
} // namespace pyrite
