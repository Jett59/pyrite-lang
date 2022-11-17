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
  virtual void visit(const VoidType &) = 0;
  virtual void visit(const IntegerType &) = 0;
  virtual void visit(const FloatType &) = 0;
  virtual void visit(const BooleanType &) = 0;
  virtual void visit(const CharType &) = 0;
  virtual void visit(const ArrayType &) = 0;
  virtual void visit(const ReferenceType &) = 0;
  virtual void visit(const FunctionType &) = 0;
  virtual void visit(const StructType &) = 0;
  virtual void visit(const UnionType &) = 0;
  virtual void visit(const EnumType &) = 0;
  virtual void visit(const IdentifiedType &) = 0;
  virtual void visit(const AnyType &) = 0;
  virtual void visit(const AutoType &) = 0;
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
class ArrayType : public Type {
public:
  ArrayType(std::unique_ptr<Type> elementType)
      : Type(TypeClass::ARRAY), elementType(std::move(elementType)) {}

  const Type &getElementType() const { return *elementType; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

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

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

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

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::unique_ptr<Type> returnType;
  std::vector<std::unique_ptr<Type>> parameters;
};
class StructType : public Type {
public:
  StructType(std::vector<NameAndType> fields, std::string name)
      : Type(TypeClass::STRUCT, std::move(name)), fields(std::move(fields)) {}

  const std::vector<NameAndType> &getFields() const { return fields; }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<NameAndType> fields;
};
class UnionType : public Type {
public:
  UnionType(std::vector<std::unique_ptr<Type>> options)
      : Type(TypeClass::UNION), options(std::move(options)) {}

  const std::vector<std::unique_ptr<Type>> &getOptions() const {
    return options;
  }

  void accept(TypeVisitor &visitor) const override { visitor.visit(*this); }

private:
  std::vector<std::unique_ptr<Type>> options;
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
  IdentifiedType(std::string name) : Type(TypeClass::IDENTIFIED, name) {}

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

// Templated prefix so it doesn't conflict with the alias.
template <typename TemplatedValueType>
class TypeTransformVisitor : public TypeVisitor {
public:
  using ValueType = TemplatedValueType;

  ValueType visit(const Type &type) {
    type.accept(*this);
    return std::move(result);
  }

  virtual ValueType visitVoid(const VoidType &type) = 0;
  void visit(const VoidType &type) override { result = visitVoid(type); }
  virtual ValueType visitInteger(const IntegerType &type) = 0;
  void visit(const IntegerType &type) override { result = visitInteger(type); }
  virtual ValueType visitFloat(const FloatType &type) = 0;
  void visit(const FloatType &type) override { result = visitFloat(type); }
  virtual ValueType visitBoolean(const BooleanType &type) = 0;
  void visit(const BooleanType &type) override { result = visitBoolean(type); }
  virtual ValueType visitChar(const CharType &type) = 0;
  void visit(const CharType &type) override { result = visitChar(type); }
  virtual ValueType visitArray(const ArrayType &type) = 0;
  void visit(const ArrayType &type) override { result = visitArray(type); }
  virtual ValueType visitReference(const ReferenceType &type) = 0;
  void visit(const ReferenceType &type) override {
    result = visitReference(type);
  }
  virtual ValueType visitFunction(const FunctionType &type) = 0;
  void visit(const FunctionType &type) override {
    result = visitFunction(type);
  }
  virtual ValueType visitStruct(const StructType &type) = 0;
  void visit(const StructType &type) override { result = visitStruct(type); }
  virtual ValueType visitUnion(const UnionType &type) = 0;
  void visit(const UnionType &type) override { result = visitUnion(type); }
  virtual ValueType visitEnum(const EnumType &type) = 0;
  void visit(const EnumType &type) override { result = visitEnum(type); }
  virtual ValueType visitIdentified(const IdentifiedType &type) = 0;
  void visit(const IdentifiedType &type) override {
    result = visitIdentified(type);
  }
  virtual ValueType visitAny(const AnyType &type) = 0;
  void visit(const AnyType &type) override { result = visitAny(type); }
  virtual ValueType visitAuto(const AutoType &type) = 0;
  void visit(const AutoType &type) override { result = visitAuto(type); }

private:
  ValueType result;
};

using TypeToTypeTransformVisitor = TypeTransformVisitor<std::unique_ptr<Type>>;

class PartialTypeToTypeTransformVisitor : public TypeToTypeTransformVisitor {
public:
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
    return std::make_unique<StructType>(std::move(fields), type.getName());
  }
  ValueType visitUnion(const UnionType &type) override {
    std::vector<std::unique_ptr<Type>> options;
    for (const auto &option : type.getOptions()) {
      options.push_back(visit(*option));
    }
    return std::make_unique<UnionType>(std::move(options));
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

void convertTypesForAssignment(std::unique_ptr<AstNode> &rhsAstNode,
                               const Type &lhs, const Type &rhs);
void convertTypesForBinaryOperator(std::unique_ptr<AstNode> &lhsAstNode,
                                   std::unique_ptr<AstNode> &rhsAstNode,
                                   const Type &lhs, const Type &rhs,
                                   const AstMetadata &expressionMetadata);
void convertTypesForUnaryOperator(std::unique_ptr<AstNode> &valueAstNode,
                                  const Type &operand,
                                  const UnaryExpressionNode &unaryExpression);
} // namespace pyrite

#endif