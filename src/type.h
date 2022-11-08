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
  BOOLEAN,
  CHAR,
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
class BooleanType;
class CharType;
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
  virtual void Visit(const BooleanType &type) = 0;
  virtual void visit(const CharType &type) = 0;
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
class BooleanType : public Type {
public:
  BooleanType() : Type(TypeClass::BOOLEAN) {}

  void accept(TypeVisitor &visitor) const override { visitor.Visit(*this); }
};
class CharType : public Type {
public:
  CharType() : Type(TypeClass::CHAR) {}

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }
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

template <typename ValueType, typename This>
class TypeTransformVisitor : public TypeVisitor {
  static_assert(std::is_base_of_v<TypeTransformVisitor<ValueType, This>, This>,
                "This must be a subclass of TypeTransformVisitor");
  static_assert(!std::is_abstract_v<This>,
                "Implementation of transformation visitor must be a concrete "
                "class; did you forget to implement one of the methods?");

public:
  ValueType visit(const Type &type) {
    type.accept(*this);
    return std::move(result);
  }

  virtual ValueType visitVoid(const VoidType &type) = 0;
  void Visit(const VoidType &type) override {
    result = static_cast<This *>(this)->visitVoid(type);
  }
  virtual ValueType visitInteger(const IntegerType &type) = 0;
  void Visit(const IntegerType &type) override {
    result = static_cast<This *>(this)->visitInteger(type);
  }
  virtual ValueType visitFloat(const FloatType &type) = 0;
  void Visit(const FloatType &type) override {
    result = static_cast<This *>(this)->visitFloat(type);
  }
  virtual ValueType visitBoolean(const BooleanType &type) = 0;
  void Visit(const BooleanType &type) override {
    result = static_cast<This *>(this)->visitBoolean(type);
  }
  virtual ValueType visitArray(const ArrayType &type) = 0;
  void Visit(const ArrayType &type) override {
    result = static_cast<This *>(this)->visitArray(type);
  }
  virtual ValueType visitReference(const ReferenceType &type) = 0;
  void Visit(const ReferenceType &type) override {
    result = static_cast<This *>(this)->visitReference(type);
  }
  virtual ValueType visitFunction(const FunctionType &type) = 0;
  void Visit(const FunctionType &type) override {
    result = static_cast<This *>(this)->visitFunction(type);
  }
  virtual ValueType visitStruct(const StructType &type) = 0;
  void Visit(const StructType &type) override {
    result = static_cast<This *>(this)->visitStruct(type);
  }
  virtual ValueType visitUnion(const UnionType &type) = 0;
  void Visit(const UnionType &type) override {
    result = static_cast<This *>(this)->visitUnion(type);
  }
  virtual ValueType visitEnum(const EnumType &type) = 0;
  void Visit(const EnumType &type) override {
    result = static_cast<This *>(this)->visitEnum(type);
  }
  virtual ValueType visitIdentified(const IdentifiedType &type) = 0;
  void Visit(const IdentifiedType &type) override {
    result = static_cast<This *>(this)->visitIdentified(type);
  }
  virtual ValueType visitAny(const AnyType &type) = 0;
  void Visit(const AnyType &type) override {
    result = static_cast<This *>(this)->visitAny(type);
  }
  virtual ValueType visitAuto(const AutoType &type) = 0;
  void Visit(const AutoType &type) override {
    result = static_cast<This *>(this)->visitAuto(type);
  }

private:
  ValueType result;
};

struct NameAndType {
  std::string name;
  std::unique_ptr<Type> type;
};

} // namespace pyrite

#endif