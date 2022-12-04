#include "mangle.h"

namespace pyrite {
class TypeNameMangler : public TypeTransformVisitor<std::string> {
public:
  std::string visitAll(std::string value, const Type &oldType) override {
    if (!oldType.getName().empty()) {
      return std::to_string(oldType.getName().size()) + oldType.getName();
    } else {
      return value;
    }
  }

  std::string visitVoid(const VoidType &) override { return "v"; }
  std::string visitInteger(const IntegerType &type) override {
    return (type.getSigned() ? "i" : "u") + std::to_string(type.getBits());
  }
  std::string visitFloat(const FloatType &type) override {
    return "f" + std::to_string(type.getBits());
  }
  std::string visitBoolean(const BooleanType &) override { return "b"; }
  std::string visitChar(const CharType &) override { return "c"; }
  std::string visitArray(const ArrayType &type) override {
    return "a" + visit(type.getElementType());
  }
  std::string visitRawArray(const RawArrayType &type) override {
    return "A" + visit(type.getElementType());
  }
  std::string visitReference(const ReferenceType &type) override {
    return "r" + visit(type.getReferencedType());
  }
  std::string visitFunction(const FunctionType &type) override {
    std::string result = "F";
    result += visit(type.getReturnType());
    for (const auto &param : type.getParameters()) {
      result += visit(*param);
    }
    result += "E";
    return result;
  }
  std::string visitStruct(const StructType &type) override {
    std::string result = "S";
    for (const auto &[fieldName, fieldType] : type.getFields()) {
      result +=
          std::to_string(fieldName.size()) + fieldName + visit(*fieldType);
    }
    result += "E";
    return result;
  }
  std::string visitUnion(const UnionType &type) override {
    std::string result = "U";
    for (const auto &option : type.getOptions()) {
      result += visit(*option);
    }
    result += "E";
    return result;
  }
  std::string visitRawUnion(const RawUnionType &type) override {
    std::string result = "UR";
    for (const auto &option : type.getOptions()) {
      result += visit(*option);
    }
    result += "E";
    return result;
  }
  std::string visitEnum(const EnumType &type) override {
    std::string result = "e";
    for (const auto &option : type.getOptions()) {
      result += std::to_string(option.size()) + option;
    }
    result += "E";
    return result;
  }
  std::string visitIdentified(const IdentifiedType &) override {
    // The type will be renamed anyway (since identified types always have a
    // name).
    return "I";
  }
  std::string visitAny(const AnyType &) override { return "y"; }
  std::string visitAuto(const AutoType &) override { return "t"; }
  std::string
  visitOverloadedFunction(const OverloadedFunctionType &type) override {
    std::string result = "o";
    for (const auto &overload : type.getOptions()) {
      result += visitFunction(*overload);
    }
    result += "E";
    return result;
  }
};

std::string mangle(const FunctionDefinitionNode &definition) {
  return mangle(definition.getName(),
                static_cast<const FunctionType &>(
                    removeReference(definition.getValueType())));
}

std::string mangle(const std::string &name, const FunctionType &type) {
  std::string result = "_P";
  result += std::to_string(name.size()) + name;
  TypeNameMangler mangler;
  result += mangler.visit(type.getReturnType());
  for (const auto &param : type.getParameters()) {
    result += mangler.visit(*param);
  }
  return result;
}
} // namespace pyrite
