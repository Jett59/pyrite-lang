#ifndef PYRITE_TYPE_H
#define PYRITE_TYPE_H

#include "helper.h"
#include <cstddef>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace pyrite {
enum class TypeClass {
  VOID,
  INTEGER,
  FLOAT,
  BOOLEAN,
  CHAR,
  ARRAY,
  RAW_ARRAY,
  REFERENCE,
  FUNCTION,
  STRUCT,
  UNION,
  RAW_UNION,
  ENUM,
  IDENTIFIED,
  ANY,
  AUTO,
  OVERLOADED_FUNCTION, // Shouldn't appear pas the type checker
};

// We have to forward-declare these because they are used in the type visitor.
class Type;
class VoidType;
class IntegerType;
class FloatType;
class BooleanType;
class CharType;
class ArrayType;
class RawArrayType;
class ReferenceType;
class FunctionType;
class StructType;
class UnionType;
class RawUnionType;
class EnumType;
class IdentifiedType;
class AnyType;
class AutoType;
class OverloadedFunctionType;

class TypeVisitor {
public:
  virtual ~TypeVisitor() = default;
  virtual void visit(const VoidType &) = 0;
  virtual void visit(const IntegerType &) = 0;
  virtual void visit(const FloatType &) = 0;
  virtual void visit(const BooleanType &) = 0;
  virtual void visit(const CharType &) = 0;
  virtual void visit(const ArrayType &) = 0;
  virtual void visit(const RawArrayType &) = 0;
  virtual void visit(const ReferenceType &) = 0;
  virtual void visit(const FunctionType &) = 0;
  virtual void visit(const StructType &) = 0;
  virtual void visit(const UnionType &) = 0;
  virtual void visit(const RawUnionType &) = 0;
  virtual void visit(const EnumType &) = 0;
  virtual void visit(const IdentifiedType &) = 0;
  virtual void visit(const AnyType &) = 0;
  virtual void visit(const AutoType &) = 0;
  virtual void visit(const OverloadedFunctionType &) = 0;
};

class Type {
public:
  Type(TypeClass typeClass) : typeClass(typeClass) {}
  virtual ~Type() = default;

  TypeClass getTypeClass() const { return typeClass; }

  // This field is set to the empty string if it doesn't matter (most types) or
  // if it is specified as a literal type (E.G. an unnamed struct).
  const std::string &getName() const { return name; }

  void setName(std::string name) { this->name = std::move(name); }

  virtual void accept(TypeVisitor &visitor) const = 0;

private:
  TypeClass typeClass;
  std::string name;
};

struct NameAndType {
  std::string name;
  std::unique_ptr<Type> type;
};

class VoidType : public Type {
public:
  VoidType() : Type(TypeClass::VOID) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class IntegerType : public Type {
public:
  IntegerType(size_t bits, bool isSigned)
      : Type(TypeClass::INTEGER), bits(bits), isSigned(isSigned) {}

  size_t getBits() const { return bits; }
  bool getSigned() const { return isSigned; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  size_t bits;
  bool isSigned;
};
static inline bool isSigned(const Type &type) {
  if (type.getTypeClass() == TypeClass::INTEGER) {
    return static_cast<const IntegerType &>(type).getSigned();
  }
  return false;
}

class FloatType : public Type {
public:
  FloatType(size_t bits) : Type(TypeClass::FLOAT), bits(bits) {}

  size_t getBits() const { return bits; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  size_t bits;
};
class BooleanType : public Type {
public:
  BooleanType() : Type(TypeClass::BOOLEAN) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class CharType : public Type {
public:
  CharType() : Type(TypeClass::CHAR) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
/**
 * @brief a raw array type
 *
 * Dispite the name, ArrayType is not like a C array; it is more like
 * a non-resizeable std::vector. RawArrayType is the equivalent of a C array and
 * is only used to simplify the ArrayType for code generation.
 */
class RawArrayType : public Type {
protected:
  RawArrayType(std::unique_ptr<Type> elementType, TypeClass typeClass)
      : Type(typeClass), elementType(std::move(elementType)) {}

public:
  RawArrayType(std::unique_ptr<Type> elementType)
      : Type(TypeClass::RAW_ARRAY), elementType(std::move(elementType)) {}

  const Type &getElementType() const { return *elementType; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<Type> elementType;
};
class ArrayType : public RawArrayType {
public:
  ArrayType(std::unique_ptr<Type> elementType)
      : RawArrayType(std::move(elementType), TypeClass::ARRAY) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class ReferenceType : public Type {
public:
  ReferenceType(std::unique_ptr<Type> referencedType, bool constant)
      : Type(TypeClass::REFERENCE), referencedType(std::move(referencedType)),
        constant(constant) {}

  const Type &getReferencedType() const { return *referencedType; }
  bool getConstant() const { return constant; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<Type> referencedType;
  bool constant;
};
static inline bool isConstantReference(const Type &type) {
  if (type.getTypeClass() == TypeClass::REFERENCE) {
    return static_cast<const ReferenceType &>(type).getConstant();
  }
  return false;
}
class FunctionType : public Type {
public:
  FunctionType(std::unique_ptr<Type> returnType,
               std::vector<std::unique_ptr<Type>> parameters)
      : Type(TypeClass::FUNCTION), returnType(std::move(returnType)),
        parameters(std::move(parameters)) {}

  const Type &getReturnType() const { return *returnType; }
  const std::vector<std::unique_ptr<Type>> &getParameters() const {
    return parameters;
  }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<Type> returnType;
  std::vector<std::unique_ptr<Type>> parameters;
};
class StructType : public Type {
public:
  StructType(std::vector<NameAndType> fields)
      : Type(TypeClass::STRUCT), fields(std::move(fields)) {}

  const std::vector<NameAndType> &getFields() const { return fields; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

  std::optional<const Type *> getMemberType(const std::string &name) const {
    for (const auto &[memberName, memberType] : fields) {
      if (memberName == name) {
        return memberType.get();
      }
    }
    return std::nullopt;
  }

private:
  std::vector<NameAndType> fields;
};
/**
 * @brief a raw union type
 *
 * Dispite the name, UnionType is not like the C union type; it is more like
 * std::variant. RawUnionType is the equivalent of a C union and is only used to
 * simplify the UnionType for code generation.
 */
class RawUnionType : public Type {
protected:
  RawUnionType(std::vector<std::unique_ptr<Type>> options, TypeClass typeClass)
      : Type(typeClass), options(std::move(options)) {}

public:
  RawUnionType(std::vector<std::unique_ptr<Type>> options)
      : Type(TypeClass::RAW_UNION), options(std::move(options)) {}

  const auto &getOptions() const { return options; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::unique_ptr<Type>> options;
};
class UnionType : public RawUnionType {
public:
  UnionType(std::vector<std::unique_ptr<Type>> options)
      : RawUnionType(std::move(options), TypeClass::UNION) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class EnumType : public Type {
public:
  EnumType(std::vector<std::string> options)
      : Type(TypeClass::ENUM), options(std::move(options)) {}

  const std::vector<std::string> &getOptions() const { return options; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::string> options;
};
class IdentifiedType : public Type {
public:
  IdentifiedType(std::string name) : Type(TypeClass::IDENTIFIED) {
    setName(std::move(name));
  }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class AnyType : public Type {
public:
  AnyType() : Type(TypeClass::ANY) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class AutoType : public Type {
public:
  AutoType() : Type(TypeClass::AUTO) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
};
class OverloadedFunctionType : public Type {
public:
  OverloadedFunctionType(std::vector<std::unique_ptr<FunctionType>> options)
      : Type(TypeClass::OVERLOADED_FUNCTION), options(std::move(options)) {}

  const std::vector<std::unique_ptr<FunctionType>> &getOptions() const {
    return options;
  }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::unique_ptr<FunctionType>> options;
};

// Templated prefix so it doesn't conflict with the alias.
template <typename TemplatedValueType>
class TypeTransformVisitor : public TypeVisitor {
public:
  using ValueType = TemplatedValueType;

  virtual ValueType visitAll(ValueType value, const Type &oldType) {
    return value;
  }

  ValueType visit(const Type &type) {
    type.accept(*this);
    return visitAll(std::move(result), type);
  }

#define IMPLEMENT_VISIT(TYPE)                                                  \
  virtual ValueType visit##TYPE(const TYPE##Type &type) = 0;                   \
  void visit(const TYPE##Type &type) override { result = visit##TYPE(type); }

  IMPLEMENT_VISIT(Void)
  IMPLEMENT_VISIT(Integer)
  IMPLEMENT_VISIT(Float)
  IMPLEMENT_VISIT(Boolean)
  IMPLEMENT_VISIT(Char)
  IMPLEMENT_VISIT(Array)
  IMPLEMENT_VISIT(RawArray)
  IMPLEMENT_VISIT(Reference)
  IMPLEMENT_VISIT(Function)
  IMPLEMENT_VISIT(Struct)
  IMPLEMENT_VISIT(RawUnion)
  IMPLEMENT_VISIT(Union)
  IMPLEMENT_VISIT(Enum)
  IMPLEMENT_VISIT(Identified)
  IMPLEMENT_VISIT(Any)
  IMPLEMENT_VISIT(Auto)
  IMPLEMENT_VISIT(OverloadedFunction)

#undef IMPLEMENT_VISIT

private:
  ValueType result;
};

using TypeToTypeTransformVisitor = TypeTransformVisitor<std::unique_ptr<Type>>;

class PartialTypeToTypeTransformVisitor : public TypeToTypeTransformVisitor {
public:
  ValueType visitAll(ValueType value, const Type &oldType) override {
    value->setName(oldType.getName());
    return value;
  }

  ValueType visitVoid(const VoidType &type) override {
    return std::make_unique<VoidType>();
  }
  ValueType visitInteger(const IntegerType &type) override {
    return std::make_unique<IntegerType>(type.getBits(), type.getSigned());
  }
  ValueType visitFloat(const FloatType &type) override {
    return std::make_unique<FloatType>(type.getBits());
  }
  ValueType visitBoolean(const BooleanType &type) override {
    return std::make_unique<BooleanType>();
  }
  ValueType visitChar(const CharType &type) override {
    return std::make_unique<CharType>();
  }
  ValueType visitRawArray(const RawArrayType &type) override {
    return std::make_unique<RawArrayType>(visit(type.getElementType()));
  }
  ValueType visitArray(const ArrayType &type) override {
    return std::make_unique<ArrayType>(visit(type.getElementType()));
  }
  ValueType visitReference(const ReferenceType &type) override {
    return std::make_unique<ReferenceType>(visit(type.getReferencedType()),
                                           type.getConstant());
  }
  ValueType visitFunction(const FunctionType &type) override {
    std::vector<std::unique_ptr<Type>> parameters;
    for (const auto &parameter : type.getParameters()) {
      parameters.push_back(visit(*parameter));
    }
    return std::make_unique<FunctionType>(visit(type.getReturnType()),
                                          std::move(parameters));
  }
  ValueType visitStruct(const StructType &type) override {
    std::vector<NameAndType> fields;
    for (const auto &field : type.getFields()) {
      fields.push_back({field.name, visit(*field.type)});
    }
    return std::make_unique<StructType>(std::move(fields));
  }
  ValueType visitUnion(const UnionType &type) override {
    std::vector<std::unique_ptr<Type>> options;
    for (const auto &option : type.getOptions()) {
      options.push_back(visit(*option));
    }
    return std::make_unique<UnionType>(std::move(options));
  }
  ValueType visitRawUnion(const RawUnionType &type) override {
    std::vector<std::unique_ptr<Type>> options;
    for (const auto &option : type.getOptions()) {
      options.push_back(visit(*option));
    }
    return std::make_unique<RawUnionType>(std::move(options));
  }
  ValueType visitEnum(const EnumType &type) override {
    return std::make_unique<EnumType>(type.getOptions());
  }
  ValueType visitIdentified(const IdentifiedType &type) override {
    return std::make_unique<IdentifiedType>(type.getName());
  }
  ValueType visitAny(const AnyType &type) override {
    return std::make_unique<AnyType>();
  }
  ValueType visitAuto(const AutoType &type) override {
    return std::make_unique<AutoType>();
  }
  ValueType
  visitOverloadedFunction(const OverloadedFunctionType &type) override {
    std::vector<std::unique_ptr<FunctionType>> options;
    for (const auto &option : type.getOptions()) {
      auto newOption = staticCast<FunctionType>(visitFunction(*option));
      options.push_back(std::move(newOption));
    }
    return std::make_unique<OverloadedFunctionType>(std::move(options));
  }
};
static_assert(!std::is_abstract_v<PartialTypeToTypeTransformVisitor>,
              "PartialTypeToTypeTransformVisitor doesn't implement all the "
              "required methods from TypeTransformVisitor");

std::unique_ptr<Type> cloneType(const Type &type);
std::string typeToString(const Type &);
bool typeEquals(const Type &a, const Type &b);

class AstNode;
class AstMetadata;
class UnaryExpressionNode;

void removeReference(const Type &type, std::unique_ptr<AstNode> &astNode);
static inline const Type &removeReference(const Type &type) {
  if (type.getTypeClass() == TypeClass::REFERENCE) {
    return static_cast<const ReferenceType &>(type).getReferencedType();
  } else {
    return type;
  }
}

bool convertTypesForAssignment(std::unique_ptr<AstNode> &rhsAstNode,
                               const Type &lhs, const Type &rhs,
                               bool emitErrors = true);
void convertTypesForBinaryOperator(std::unique_ptr<AstNode> &lhsAstNode,
                                   std::unique_ptr<AstNode> &rhsAstNode,
                                   const Type &lhs, const Type &rhs,
                                   const AstMetadata &expressionMetadata);
void convertTypesForUnaryOperator(std::unique_ptr<AstNode> &valueAstNode,
                                  const Type &operand,
                                  const UnaryExpressionNode &unaryExpression);
} // namespace pyrite

#endif