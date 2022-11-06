#ifndef PYRITE_TYPE_H
#define PYRITE_TYPE_H

#include <cstddef>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace pyrite {
enum class TypeClass {
  VOID,
  INTEGER,
  FLOAT,
  ARRAY,
  REFERENCE,
  FUNCTION,
  STRUCT,
  UNION,
  ENUM,
  IDENTIFIED,
  ANY,
  AUTO,
};

// We have to forward-declare these because they are used in the type visitor.
class Type;
class VoidType;
class IntegerType;
class FloatType;
class ArrayType;
class ReferenceType;
class FunctionType;
class StructType;
class UnionType;
class EnumType;
class IdentifiedType;
class AnyType;
class AutoType;

class TypeVisitor {
public:
  virtual ~TypeVisitor() = default;
  virtual void Visit(const VoidType &type) = 0;
  virtual void Visit(const IntegerType &type) = 0;
  virtual void Visit(const FloatType &type) = 0;
  virtual void Visit(const ArrayType &type) = 0;
  virtual void Visit(const ReferenceType &type) = 0;
  virtual void Visit(const FunctionType &type) = 0;
  virtual void Visit(const StructType &type) = 0;
  virtual void Visit(const UnionType &type) = 0;
  virtual void Visit(const EnumType &type) = 0;
  virtual void Visit(const IdentifiedType &type) = 0;
  virtual void Visit(const AnyType &type) = 0;
  virtual void Visit(const AutoType &type) = 0;
};

class Type {
public:
  Type(TypeClass typeClass, std::string name = "") : typeClass(typeClass) {}
  virtual ~Type() = default;

  TypeClass getTypeClass() const { return typeClass; }

  // This field is set to the empty string if it doesn't matter (most types) or
  // if it is specified as a literal type (E.G. an unnamed struct).
  const std::string &getName() const { return name; }

  virtual void accept(TypeVisitor &visitor) const = 0;

private:
  TypeClass typeClass;
  std::string name;
};
class VoidType : public Type {
public:
  VoidType() : Type(TypeClass::VOID) {}

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }
};
class IntegerType : public Type {
public:
  IntegerType(size_t bits, bool isSigned)
      : Type(TypeClass::INTEGER), bits(bits), isSigned(isSigned) {}

  size_t getBits() const { return bits; }
  bool getIsSigned() const { return isSigned; }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  size_t bits;
  bool isSigned;
};
class FloatType : public Type {
public:
  FloatType(size_t bits) : Type(TypeClass::FLOAT), bits(bits) {}

  size_t getBits() const { return bits; }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  size_t bits;
};
class ArrayType : public Type {
public:
  ArrayType(std::unique_ptr<Type> elementType)
      : Type(TypeClass::ARRAY), elementType(std::move(elementType)) {}

  const Type &getElementType() const { return *elementType; }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  std::unique_ptr<Type> elementType;
};
class ReferenceType : public Type {
public:
  ReferenceType(std::unique_ptr<Type> referencedType, bool constant)
      : Type(TypeClass::REFERENCE), referencedType(std::move(referencedType)),
        constant(constant) {}

  const Type &getReferencedType() const { return *referencedType; }
  bool getConstant() const { return constant; }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  std::unique_ptr<Type> referencedType;
  bool constant;
};
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

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  std::unique_ptr<Type> returnType;
  std::vector<std::unique_ptr<Type>> parameters;
};
class StructType : public Type {
public:
  StructType(std::map<std::string, std::unique_ptr<Type>> fields,
             std::string name)
      : Type(TypeClass::STRUCT, std::move(name)), fields(std::move(fields)) {}

  const std::map<std::string, std::unique_ptr<Type>> &getFields() const {
    return fields;
  }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  std::map<std::string, std::unique_ptr<Type>> fields;
};
class UnionType : public Type {
public:
  UnionType(std::map<std::string, std::unique_ptr<Type>> options)
      : Type(TypeClass::UNION), options(std::move(options)) {}

  const std::map<std::string, std::unique_ptr<Type>> &getOptions() const {
    return options;
  }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  std::map<std::string, std::unique_ptr<Type>> options;
};
class EnumType : public Type {
public:
  EnumType(std::map<std::string, int64_t> options, std::string name)
      : Type(TypeClass::ENUM, std::move(name)), options(std::move(options)) {}

  const std::map<std::string, int64_t> &getOptions() const { return options; }

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }

private:
  std::map<std::string, int64_t> options;
};
class IdentifiedType : public Type {
public:
  IdentifiedType(std::string name) : Type(TypeClass::IDENTIFIED, name) {}

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }
};
class AnyType : public Type {
public:
  AnyType() : Type(TypeClass::ANY) {}

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }
};
class AutoType : public Type {
public:
  AutoType() : Type(TypeClass::AUTO) {}

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }
};
// Check it has visitVoid, visitInteger, visitFloat, visitArray, visitReference,
// visitFunction, visitStruct, visitUnion, visitEnum, visitIdentified, visitAny,
// visitAuto and they all return the right type.
template <typename Implementation, typename ValueType>
concept TypeTransformVisitorImplementation =
    requires(Implementation impl, const VoidType &voidType,
             const IntegerType &integerType, const FloatType &floatType,
             const ArrayType &arrayType, const ReferenceType &referenceType,
             const FunctionType &functionType, const StructType &structType,
             const UnionType &unionType, const EnumType &enumType,
             const IdentifiedType &identifiedType, const AnyType &anyType,
             const AutoType &autoType) {
  { impl.visitVoid(voidType) } -> std::same_as<ValueType>;
  { impl.visitInteger(integerType) } -> std::same_as<ValueType>;
  { impl.visitFloat(floatType) } -> std::same_as<ValueType>;
  { impl.visitArray(arrayType) } -> std::same_as<ValueType>;
  { impl.visitReference(referenceType) } -> std::same_as<ValueType>;
  { impl.visitFunction(functionType) } -> std::same_as<ValueType>;
  { impl.visitStruct(structType) } -> std::same_as<ValueType>;
  { impl.visitUnion(unionType) } -> std::same_as<ValueType>;
  { impl.visitEnum(enumType) } -> std::same_as<ValueType>;
  { impl.visitIdentified(identifiedType) } -> std::same_as<ValueType>;
  { impl.visitAny(anyType) } -> std::same_as<ValueType>;
  { impl.visitAuto(autoType) } -> std::same_as<ValueType>;
};

template <typename ValueType,
          TypeTransformVisitorImplementation<ValueType> This>
class TypeTransformVisitor : public TypeVisitor {
public:
  ValueType visit(const Type &type) {
    type.accept(*this);
    return std::move(result);
  }

  void Visit(const VoidType &type) override {
    result = static_cast<This *>(this)->visitVoid(type);
  }
  void Visit(const IntegerType &type) override {
    result = static_cast<This *>(this)->visitInteger(type);
  }
  void Visit(const FloatType &type) override {
    result = static_cast<This *>(this)->visitFloat(type);
  }
  void Visit(const ArrayType &type) override {
    result = static_cast<This *>(this)->visitArray(type);
  }
  void Visit(const ReferenceType &type) override {
    result = static_cast<This *>(this)->visitReference(type);
  }
  void Visit(const FunctionType &type) override {
    result = static_cast<This *>(this)->visitFunction(type);
  }
  void Visit(const StructType &type) override {
    result = static_cast<This *>(this)->visitStruct(type);
  }
  void Visit(const UnionType &type) override {
    result = static_cast<This *>(this)->visitUnion(type);
  }
  void Visit(const EnumType &type) override {
    result = static_cast<This *>(this)->visitEnum(type);
  }
  void Visit(const IdentifiedType &type) override {
    result = static_cast<This *>(this)->visitIdentified(type);
  }
  void Visit(const AnyType &type) override {
    result = static_cast<This *>(this)->visitAny(type);
  }
  void Visit(const AutoType &type) override {
    result = static_cast<This *>(this)->visitAuto(type);
  }

private:
  ValueType result;
};
} // namespace pyrite

#endif