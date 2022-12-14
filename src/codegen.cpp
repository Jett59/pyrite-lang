#include "codegen.h"
#include "optimizations.h"
#include <iostream>
#include <llvm/Bitcode/BitcodeWriterPass.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace llvm;

namespace pyrite {
class CodegenAstVisitor : public AstTransformerVisitor<Value *> {
  class PyriteTypeToLLVMTypeTransformer
      : public TypeTransformVisitor<llvm::Type *> {
  public:
    PyriteTypeToLLVMTypeTransformer(LLVMContext &context, DataLayout dataLayout)
        : context(context), dataLayout(dataLayout) {}

    ValueType visitVoid(const VoidType &) override {
      return llvm::Type::getVoidTy(context);
    }
    ValueType visitInteger(const IntegerType &type) override {
      return llvm::Type::getIntNTy(context, type.getBits());
    }
    ValueType visitFloat(const FloatType &type) override {
      switch (type.getBits()) {
      case 32:
        return llvm::Type::getFloatTy(context);
      case 64:
        return llvm::Type::getDoubleTy(context);
      default:
        throw std::runtime_error("Unsupported float type");
      }
    }
    ValueType visitBoolean(const BooleanType &) override {
      return llvm::Type::getInt1Ty(context);
    }
    ValueType visitChar(const CharType &) override {
      return llvm::Type::getInt32Ty(context);
    }
    ValueType visitArray(const ArrayType &) override {
      throw std::runtime_error("Array type not supported in code generator");
    }
    ValueType visitRawArray(const RawArrayType &type) override {
      return visit(type.getElementType())->getPointerTo();
    }
    ValueType visitReference(const ReferenceType &type) override {
      return visit(type.getReferencedType())->getPointerTo();
    }
    ValueType visitFunction(const FunctionType &type) override {
      std::vector<llvm::Type *> parameterTypes;
      for (const auto &parameter : type.getParameters()) {
        parameterTypes.push_back(visit(*parameter));
      }
      return llvm::FunctionType::get(visit(type.getReturnType()),
                                     parameterTypes, false);
    }
    ValueType visitStruct(const StructType &type) override {
      std::vector<llvm::Type *> fieldTypes;
      for (const auto &[name, field] : type.getFields()) {
        fieldTypes.push_back(visit(*field));
      }
      return llvm::StructType::get(context, fieldTypes);
    }
    ValueType visitUnion(const UnionType &) override {
      throw std::runtime_error("Union type not supported in code generator");
    }
    ValueType visitRawUnion(const RawUnionType &type) override {
      if (type.getOptions().size() == 0) {
        // The general implementation segfaults when there are no options. This
        // is a workaround.
        return llvm::StructType::get(context);
      }
      std::vector<llvm::Type *> optionTypes;
      for (const auto &option : type.getOptions()) {
        optionTypes.push_back(visit(*option));
      }
      llvm::Type *maxAlignmentType = optionTypes[0];
      for (auto type : optionTypes) {
        if (dataLayout.getABITypeAlignment(type) >
            dataLayout.getABITypeAlignment(maxAlignmentType)) {
          maxAlignmentType = type;
        }
      }
      llvm::Type *maxSizeType = optionTypes[0];
      for (auto type : optionTypes) {
        if (dataLayout.getTypeAllocSize(type) >
            dataLayout.getTypeAllocSize(maxSizeType)) {
          maxSizeType = type;
        }
      }
      size_t paddingBytes = dataLayout.getTypeAllocSize(maxSizeType) -
                            dataLayout.getTypeAllocSize(maxAlignmentType);
      return llvm::StructType::get(
          context,
          {maxAlignmentType,
           llvm::ArrayType::get(llvm::Type::getInt8Ty(context), paddingBytes)});
    }
    ValueType visitEnum(const EnumType &) override {
      throw std::runtime_error("Enum type not supported in code generator");
    }
    ValueType visitIdentified(const IdentifiedType &) override {
      throw std::runtime_error(
          "Identified type not supported in code generator");
    }
    ValueType visitAny(const AnyType &) override {
      throw std::runtime_error("Any type not supported in code generator");
    }
    ValueType visitAuto(const AutoType &) override {
      throw std::runtime_error("Auto type not supported in code generator");
    }
    ValueType visitOverloadedFunction(const OverloadedFunctionType &) override {
      throw std::runtime_error(
          "Overloaded function type not supported in code generator");
    }

    LLVMContext &context;
    DataLayout dataLayout;
  };

public:
  CodegenAstVisitor(Module &module, LLVMContext &context,
                    TargetMachine *targetMachine)
      : module(module), context(context), targetMachine(targetMachine),
        irBuilder(context),
        pyriteToLLVMTypeTransformer(context, targetMachine->createDataLayout()),
        globalConstructorIrBuilder(context) {
    createVariableScope();
    globalConstructorFunction = Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), false),
        GlobalValue::InternalLinkage, "$global_constructors", module);
    BasicBlock *globalConstructorEntryBlock =
        BasicBlock::Create(context, "entry", globalConstructorFunction);
    globalConstructorIrBuilder.SetInsertPoint(globalConstructorEntryBlock);
  }

  void postCodegen() {
    globalConstructorIrBuilder.CreateRetVoid();
    // Use the @llvm.global_ctors global variable to run the global constructor
    // function. This code is slightly messy.
    llvm::StructType *globalConstructorType =
        llvm::StructType::get(context, {llvm::Type::getInt32Ty(context),
                                        globalConstructorFunction->getType(),
                                        llvm::Type::getInt8PtrTy(context)});
    Constant *globalConstructorFunctionConstant = globalConstructorFunction;
    Constant *globalConstructorPriorityConstant =
        ConstantInt::get(llvm::Type::getInt32Ty(context), 65535);
    Constant *globalConstructor = ConstantStruct::get(
        globalConstructorType,
        {globalConstructorPriorityConstant, globalConstructorFunctionConstant,
         ConstantPointerNull::get(llvm::Type::getInt8PtrTy(context))});
    Constant *globalConstructorArray = ConstantArray::get(
        llvm::ArrayType::get(globalConstructorType, 1), {globalConstructor});
    new GlobalVariable(module, globalConstructorArray->getType(), true,
                       GlobalValue::AppendingLinkage, globalConstructorArray,
                       "llvm.global_ctors");
  }

  struct VariableInfo {
    Value *value;
    const pyrite::Type &type;
  };

  ValueType visitCompilationUnit(const CompilationUnitNode &node) override {
    for (const auto &definition : node.getDefinitions()) {
      visit(*definition);
    }
    return nullptr;
  }
  ValueType
  visitVariableDefinition(const VariableDefinitionNode &node) override {
    Value *variable;
    if (function) {
      // Add an alloca to the top of the function's entry block.
      IRBuilder<> tmpBuilder(&function->getEntryBlock(),
                             function->getEntryBlock().begin());
      auto variableType = getLLVMType(*node.getType());
      variable = tmpBuilder.CreateAlloca(variableType);
      irBuilder.CreateLifetimeStart(variable, getSizeOf(variableType));
      irBuilder.CreateStore(visit(*node.getInitializer()), variable);
      if (!node.getMutable()) {
        irBuilder.CreateInvariantStart(variable, getSizeOf(variableType));
      }
    } else {
      variable = new GlobalVariable(
          module, getLLVMType(*node.getType()), false,
          GlobalValue::InternalLinkage,
          Constant::getNullValue(getLLVMType(*node.getType())), node.getName());
      // We don't need to save and restore the IP of the ir builder because we
      // are at global scope and it isn't pointing to anything important anyway.
      // This is a hack to get one IRBuilder to point to the end of another.
      irBuilder.restoreIP(globalConstructorIrBuilder.saveIP());
      globalConstructorIrBuilder.CreateStore(visit(*node.getInitializer()),
                                             variable);
      // And we have to do it in reverse to update the global constructor
      // builder.
      globalConstructorIrBuilder.restoreIP(irBuilder.saveIP());
    }
    variables.back().insert({node.getName(), {variable, *node.getType()}});
    // Return the variable as if it were just referenced by a variable
    // reference.
    return visitVariableReference(
        VariableReferenceNode{node.getName(), node.getMetadata().clone()});
  }
  ValueType
  visitFunctionDefinition(const FunctionDefinitionNode &node) override {
    Function *oldFunction = function;
    function = Function::Create(
        static_cast<llvm::FunctionType *>(pyriteToLLVMTypeTransformer.visit(
            removeReference(node.getValueType()))),
        node.getCExported() ? GlobalValue::ExternalLinkage
                            : GlobalValue::InternalLinkage,
        node.getName(), module);
    variables.back().insert(
        {node.getName(), {function, removeReference(node.getValueType())}});
    BasicBlock *entryBlock = BasicBlock::Create(context, "entry", function);
    auto previousInsertPoint = irBuilder.saveIP();
    irBuilder.SetInsertPoint(entryBlock);
    createVariableScope();
    for (size_t i = 0; i < node.getParameters().size(); i++) {
      const auto &parameter = node.getParameters()[i];
      Value *rawParameter = function->arg_begin() + i;
      rawParameter->setName(parameter.name);
      auto parameterType = getLLVMType(*parameter.type);
      Value *parameterVariable = irBuilder.CreateAlloca(parameterType);
      irBuilder.CreateStore(rawParameter, parameterVariable);
      irBuilder.CreateInvariantStart(parameterVariable,
                                     getSizeOf(parameterType));
      variables.back().insert(
          {parameter.name, {parameterVariable, *parameter.type}});
    }
    visit(*node.getBody());
    destroyVariableScope();
    function = oldFunction;
    irBuilder.restoreIP(previousInsertPoint);
    return nullptr;
  }
  ValueType visitIntegerLiteral(const IntegerLiteralNode &node) override {
    if (node.getValue() < 0) {
      return ConstantInt::getSigned(getLLVMType(node.getValueType()),
                                    node.getValue());
    } else {
      return ConstantInt::get(getLLVMType(node.getValueType()),
                              node.getValue());
    }
  }
  ValueType visitFloatLiteral(const FloatLiteralNode &node) override {
    return ConstantFP::get(getLLVMType(node.getValueType()), node.getValue());
  }
  ValueType visitStringLiteral(const StringLiteralNode &) override {
    throw std::runtime_error("String literals not supported in code generator");
  }
  ValueType visitBooleanLiteral(const BooleanLiteralNode &node) override {
    return ConstantInt::get(getLLVMType(node.getValueType()), node.getValue());
  }
  ValueType visitCharLiteral(const CharLiteralNode &node) override {
    return ConstantInt::get(getLLVMType(node.getValueType()), node.getValue());
  }
  ValueType visitReturnStatement(const ReturnStatementNode &node) override {
    if (node.getExpression()) {
      irBuilder.CreateRet(visit(**node.getExpression()));
    } else {
      irBuilder.CreateRetVoid();
    }
    return nullptr;
  }
  ValueType visitBlockStatement(const BlockStatementNode &node) override {
    createVariableScope();
    for (const auto &statement : node.getStatements()) {
      visit(*statement);
    }
    destroyVariableScope();
    return nullptr;
  }
  ValueType visitIfStatement(const IfStatementNode &node) override {
    BasicBlock *thenBlock = BasicBlock::Create(context, "then", function);
    BasicBlock *elseBlock = BasicBlock::Create(context, "else", function);
    BasicBlock *mergeBlock = BasicBlock::Create(context, "merge", function);
    irBuilder.CreateCondBr(visit(*node.getCondition()), thenBlock, elseBlock);
    irBuilder.SetInsertPoint(thenBlock);
    visit(*node.getThenStatement());
    if (!node.getThenStatement()->getMetadata().alwaysReturns) {
      irBuilder.CreateBr(mergeBlock);
    }
    thenBlock = irBuilder.GetInsertBlock();
    irBuilder.SetInsertPoint(elseBlock);
    if (node.getElseStatement()) {
      visit(*node.getElseStatement());
    }
    if (!node.getElseStatement() ||
        !node.getElseStatement()->getMetadata().alwaysReturns) {
      irBuilder.CreateBr(mergeBlock);
    }
    elseBlock = irBuilder.GetInsertBlock();
    irBuilder.SetInsertPoint(mergeBlock);
    return nullptr;
  }
  ValueType visitWhileStatement(const WhileStatementNode &node) override {
    BasicBlock *conditionBlock =
        BasicBlock::Create(context, "condition", function);
    BasicBlock *bodyBlock = BasicBlock::Create(context, "body", function);
    BasicBlock *mergeBlock = BasicBlock::Create(context, "merge", function);
    irBuilder.CreateBr(conditionBlock);
    irBuilder.SetInsertPoint(conditionBlock);
    irBuilder.CreateCondBr(visit(*node.getCondition()), bodyBlock, mergeBlock);
    irBuilder.SetInsertPoint(bodyBlock);
    visit(*node.getBody());
    if (!node.getBody()->getMetadata().alwaysReturns) {
      irBuilder.CreateBr(conditionBlock);
    }
    bodyBlock = irBuilder.GetInsertBlock();
    irBuilder.SetInsertPoint(mergeBlock);
    return nullptr;
  }
  ValueType generateBinaryExpression(Value *left, Value *right,
                                     BinaryOperator op,
                                     const pyrite::Type &operandType) {
    switch (op) {
    case BinaryOperator::ADD: {
      if (left->getType()->isIntegerTy()) {
        return irBuilder.CreateAdd(left, right);
      } else {
        return irBuilder.CreateFAdd(left, right);
      }
    }
    case BinaryOperator::SUBTRACT: {
      if (left->getType()->isIntegerTy()) {
        return irBuilder.CreateSub(left, right);
      } else {
        return irBuilder.CreateFSub(left, right);
      }
    }
    case BinaryOperator::MULTIPLY: {
      if (left->getType()->isIntegerTy()) {
        return irBuilder.CreateMul(left, right);
      } else {
        return irBuilder.CreateFMul(left, right);
      }
    }
    case BinaryOperator::DIVIDE: {
      if (left->getType()->isIntegerTy()) {
        if (isSigned(operandType)) {
          return irBuilder.CreateSDiv(left, right);
        } else {
          return irBuilder.CreateUDiv(left, right);
        }
      } else {
        return irBuilder.CreateFDiv(left, right);
      }
    }
    case BinaryOperator::MODULO: {
      if (left->getType()->isIntegerTy()) {
        if (isSigned(operandType)) {
          return irBuilder.CreateSRem(left, right);
        } else {
          return irBuilder.CreateURem(left, right);
        }
      } else {
        return irBuilder.CreateFRem(left, right);
      }
    }
    case BinaryOperator::EQUAL: {
      if (left->getType()->isIntegerTy()) {
        return irBuilder.CreateICmpEQ(left, right);
      } else {
        return irBuilder.CreateFCmpOEQ(left, right);
      }
    }
    case BinaryOperator::NOT_EQUAL: {
      if (left->getType()->isIntegerTy()) {
        return irBuilder.CreateICmpNE(left, right);
      } else {
        return irBuilder.CreateFCmpONE(left, right);
      }
    }
    case BinaryOperator::LESS_THAN: {
      if (left->getType()->isIntegerTy()) {
        if (isSigned(operandType)) {
          return irBuilder.CreateICmpSLT(left, right);
        } else {
          return irBuilder.CreateICmpULT(left, right);
        }
      } else {
        return irBuilder.CreateFCmpOLT(left, right);
      }
    }
    case BinaryOperator::LESS_THAN_OR_EQUAL: {
      if (left->getType()->isIntegerTy()) {
        if (isSigned(operandType)) {
          return irBuilder.CreateICmpSLE(left, right);
        } else {
          return irBuilder.CreateICmpULE(left, right);
        }
      } else {
        return irBuilder.CreateFCmpOLE(left, right);
      }
    }
    case BinaryOperator::GREATER_THAN: {
      if (left->getType()->isIntegerTy()) {
        if (isSigned(operandType)) {
          return irBuilder.CreateICmpSGT(left, right);
        } else {
          return irBuilder.CreateICmpUGT(left, right);
        }
      } else {
        return irBuilder.CreateFCmpOGT(left, right);
      }
    }
    case BinaryOperator::GREATER_THAN_OR_EQUAL: {
      if (left->getType()->isIntegerTy()) {
        if (isSigned(operandType)) {
          return irBuilder.CreateICmpSGE(left, right);
        } else {
          return irBuilder.CreateICmpUGE(left, right);
        }
      } else {
        return irBuilder.CreateFCmpOGE(left, right);
      }
    }
    case BinaryOperator::BITWISE_AND: {
      return irBuilder.CreateAnd(left, right);
    }
    case BinaryOperator::BITWISE_OR: {
      return irBuilder.CreateOr(left, right);
    }
    case BinaryOperator::BITWISE_XOR: {
      return irBuilder.CreateXor(left, right);
    }
    case BinaryOperator::BITWISE_SHIFT_LEFT: {
      return irBuilder.CreateShl(left, right);
    }
    case BinaryOperator::BITWISE_SHIFT_RIGHT: {
      if (isSigned(operandType)) {
        return irBuilder.CreateAShr(left, right);
      } else {
        return irBuilder.CreateLShr(left, right);
      }
    }
    }
    throw std::runtime_error("Unknown binary operator");
  }

  ValueType visitBinaryExpression(const BinaryExpressionNode &node) override {
    if (node.getOp() == BinaryOperator::LOGICAL_AND ||
        node.getOp() == BinaryOperator::LOGICAL_OR) {
      BasicBlock *leftCheck =
          BasicBlock::Create(context, "leftCheck", function);
      irBuilder.CreateBr(leftCheck);
      irBuilder.SetInsertPoint(leftCheck);
      Value *leftValue = visit(*node.getLeft());
      BasicBlock *rightCheck =
          BasicBlock::Create(context, "rightCheck", function);
      BasicBlock *merge = BasicBlock::Create(context, "merge", function);
      if (node.getOp() == BinaryOperator::LOGICAL_AND) {
        irBuilder.CreateCondBr(leftValue, rightCheck, merge);
      } else {
        irBuilder.CreateCondBr(leftValue, merge, rightCheck);
      }
      irBuilder.SetInsertPoint(rightCheck);
      Value *rightValue = visit(*node.getRight());
      irBuilder.CreateBr(merge);
      irBuilder.SetInsertPoint(merge);
      PHINode *phi = irBuilder.CreatePHI(leftValue->getType(), 2);
      phi->addIncoming(leftValue, leftCheck);
      phi->addIncoming(rightValue, rightCheck);
      return phi;
    }
    Value *left = visit(*node.getLeft());
    Value *right = visit(*node.getRight());
    return generateBinaryExpression(left, right, node.getOp(),
                                    node.getLeft()->getValueType());
  }
  ValueType visitUnaryExpression(const UnaryExpressionNode &node) override {
    Value *operand = visit(*node.getOperand());
    switch (node.getOp()) {
    case UnaryOperator::NEGATE: {
      if (operand->getType()->isIntegerTy()) {
        return irBuilder.CreateNeg(operand);
      } else {
        return irBuilder.CreateFNeg(operand);
      }
    }
    case UnaryOperator::BITWISE_NOT: {
      return irBuilder.CreateNot(operand);
    }
    case UnaryOperator::LOGICAL_NOT: {
      return irBuilder.CreateNot(operand);
    }
    case UnaryOperator::PREFIX_INCREMENT: {
      Value *dereferencedOperand = irBuilder.CreateLoad(
          getLLVMType(removeReference(node.getOperand()->getValueType())),
          operand);
      Value *one = ConstantInt::get(dereferencedOperand->getType(), 1);
      Value *result = irBuilder.CreateAdd(dereferencedOperand, one);
      irBuilder.CreateStore(result, operand);
      return result;
    }
    case UnaryOperator::PREFIX_DECREMENT: {
      Value *dereferencedOperand = irBuilder.CreateLoad(
          getLLVMType(removeReference(node.getOperand()->getValueType())),
          operand);
      Value *one = ConstantInt::get(dereferencedOperand->getType(), 1);
      Value *result = irBuilder.CreateSub(dereferencedOperand, one);
      irBuilder.CreateStore(result, operand);
      return result;
    }
    case UnaryOperator::POSTFIX_INCREMENT: {
      Value *dereferencedOperand = irBuilder.CreateLoad(
          getLLVMType(removeReference(node.getOperand()->getValueType())),
          operand);
      Value *one = ConstantInt::get(dereferencedOperand->getType(), 1);
      Value *result = irBuilder.CreateAdd(dereferencedOperand, one);
      irBuilder.CreateStore(result, operand);
      return dereferencedOperand;
    }
    case UnaryOperator::POSTFIX_DECREMENT: {
      Value *dereferencedOperand = irBuilder.CreateLoad(
          getLLVMType(removeReference(node.getOperand()->getValueType())),
          operand);
      Value *one = ConstantInt::get(dereferencedOperand->getType(), 1);
      Value *result = irBuilder.CreateSub(dereferencedOperand, one);
      irBuilder.CreateStore(result, operand);
      return dereferencedOperand;
    }
    }
    throw std::runtime_error("Unknown unary operator");
  }
  ValueType visitVariableReference(const VariableReferenceNode &node) override {
    for (auto iterator = variables.rbegin(); iterator != variables.rend();
         iterator++) {
      auto &variableTable = *iterator;
      if (variableTable.contains(node.getName())) {
        VariableInfo variableInfo = variableTable.at(node.getName());
        Value *variableValue = variableInfo.value;
        // If the variable holds a reference we must dereference it now.
        if (variableInfo.type.getTypeClass() == TypeClass::REFERENCE) {
          return irBuilder.CreateLoad(getLLVMType(node.getValueType()),
                                      variableValue);
        } else {
          return variableValue;
        }
      }
    }
    throw std::runtime_error("Variable not found: " + node.getName());
  }
  ValueType visitFunctionCall(const FunctionCallNode &node) override {
    std::vector<ValueType> arguments;
    for (const auto &argument : node.getArguments()) {
      arguments.push_back(visit(*argument));
    }
    Value *function = visit(*node.getFunction());
    return irBuilder.CreateCall(
        static_cast<llvm::FunctionType *>(
            getLLVMType(removeReference(node.getFunction()->getValueType()))),
        function, arguments);
  }
  ValueType visitAssignment(const AssignmentNode &node) override {
    Value *left = visit(*node.getLhs());
    Value *right = visit(*node.getRhs());
    if (node.getAdditionalOperator()) {
      Value *dereferencedLeft = irBuilder.CreateLoad(right->getType(), left);
      right = generateBinaryExpression(dereferencedLeft, right,
                                       *node.getAdditionalOperator(),
                                       node.getRhs()->getValueType());
    }
    irBuilder.CreateStore(right, left);
    return left;
  }
  ValueType visitDereference(const DereferenceNode &node) {
    return irBuilder.CreateLoad(getLLVMType(node.getValueType()),
                                visit(*node.getValue()));
  }
  ValueType visitCast(const CastNode &node) override {
    Value *value = visit(*node.getValue());
    const Type &valueType = node.getValue()->getValueType();
    const Type &resultType = *node.getType();
    auto llvmValueType = getLLVMType(valueType);
    auto llvmResultType = getLLVMType(resultType);
    auto valueTypeClass = valueType.getTypeClass();
    auto resultTypeClass = resultType.getTypeClass();
    if (valueTypeClass == TypeClass::INTEGER &&
        resultTypeClass == TypeClass::INTEGER) {
      return irBuilder.CreateIntCast(value, llvmResultType,
                                     isSigned(resultType));
    } else if (valueTypeClass == TypeClass::FLOAT &&
               resultTypeClass == TypeClass::FLOAT) {
      return irBuilder.CreateFPCast(value, llvmResultType);
    } else if (valueTypeClass == TypeClass::INTEGER &&
               resultTypeClass == TypeClass::FLOAT) {
      if (isSigned(valueType)) {
        return irBuilder.CreateSIToFP(value, llvmResultType);
      } else {
        return irBuilder.CreateUIToFP(value, llvmResultType);
      }
    } else if (valueTypeClass == TypeClass::FLOAT &&
               resultTypeClass == TypeClass::INTEGER) {
      if (isSigned(resultType)) {
        return irBuilder.CreateFPToSI(value, llvmResultType);
      } else {
        return irBuilder.CreateFPToUI(value, llvmResultType);
      }
    } else if (resultTypeClass == TypeClass::RAW_UNION) {
      auto temporary = createTemporaryVariable(llvmResultType, false);
      irBuilder.CreateStore(
          value,
          irBuilder.CreateBitCast(temporary, value->getType()->getPointerTo()));
      auto result = irBuilder.CreateLoad(llvmResultType, temporary);
      destroyTemporaryVariable(temporary);
      return result;
    } else {
      return irBuilder.CreateBitCast(value, llvmResultType);
    }
  }

  size_t getStructFieldIndex(const StructType &type, const std::string &name) {
    for (size_t i = 0; i < type.getFields().size(); i++) {
      const auto &[fieldName, fieldType] = type.getFields()[i];
      if (fieldName == name) {
        return i;
      }
    }
    throw std::runtime_error("Struct member not found");
  }

  ValueType visitStructLiteral(const StructLiteralNode &node) override {
    const StructType &valueType =
        static_cast<const StructType &>(node.getValueType());
    auto structType = static_cast<llvm::StructType *>(getLLVMType(valueType));
    Value *result =
        ConstantStruct::get(structType, Constant::getNullValue(structType));
    for (const auto &[name, value] : node.getValues()) {
      result = irBuilder.CreateInsertValue(
          result, visit(*value), getStructFieldIndex(valueType, name));
    }
    return result;
  }
  ValueType visitArrayLiteral(const ArrayLiteralNode &) override {
    throw std::runtime_error("Array literal not supported in code generator");
  }
  ValueType visitRawArrayLiteral(const RawArrayLiteralNode &node) override {
    const RawArrayType &valueType =
        static_cast<const RawArrayType &>(node.getValueType());
    auto elementType = getLLVMType(valueType.getElementType());
    auto arrayType = llvm::ArrayType::get(elementType, node.getValues().size());
    auto arrayVariable = createTemporaryVariable(arrayType);
    for (size_t i = 0; i < node.getValues().size(); i++) {
      auto value = visit(*node.getValues()[i]);
      auto llvmIndex = ConstantInt::get(llvm::Type::getInt64Ty(context), i);
      auto pointer = irBuilder.CreateGEP(
          arrayType, arrayVariable,
          {ConstantInt::get(llvm::Type::getInt64Ty(context), 0), llvmIndex});
      irBuilder.CreateStore(value, pointer);
    }
    return irBuilder.CreateBitCast(arrayVariable, getLLVMType(valueType));
  }
  ValueType visitStructMember(const StructMemberNode &node) override {
    const StructType &structType = static_cast<const StructType &>(
        removeReference(node.getStructValue()->getValueType()));
    unsigned fieldIndex = static_cast<unsigned>(
        getStructFieldIndex(structType, node.getMember()));
    Value *value = visit(*node.getStructValue());
    return irBuilder.CreateStructGEP(getLLVMType(structType), value,
                                     fieldIndex);
  }
  ValueType visitArrayIndex(const ArrayIndexNode &) override {
    throw std::runtime_error("Array index not supported in code generator");
  }
  ValueType visitRawArrayIndex(const RawArrayIndexNode &node) override {
    Value *array = visit(*node.getArray());
    Value *index = visit(*node.getIndex());
    const RawArrayType &arrayType =
        static_cast<const RawArrayType &>(node.getArray()->getValueType());
    auto elementType = getLLVMType(arrayType.getElementType());
    auto pointer = irBuilder.CreateInBoundsGEP(elementType, array, {index});
    return pointer;
  }
  ValueType visitAssert(const AssertNode &node) override {
    Value *lhs = visit(*node.getLhs());
    Value *rhs = visit(*node.getRhs());
    Value *condition =
        generateBinaryExpression(lhs, rhs, node.getOp(), node.getValueType());
    auto continueBlock =
        llvm::BasicBlock::Create(context, "continue", function);
    auto failBlock = llvm::BasicBlock::Create(context, "fail", function);
    irBuilder.CreateCondBr(condition, continueBlock, failBlock);
    irBuilder.SetInsertPoint(failBlock);
    visit(*node.getPanic());
    // Just in case the panic statement doesn't do it, trap for good measure.
    irBuilder.CreateCall(
        llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::trap));
    irBuilder.CreateUnreachable();
    irBuilder.SetInsertPoint(continueBlock);
    return node.getUseLhs() ? lhs : rhs;
  }
  ValueType visitExternalFunction(const ExternalFunctionNode &node) {
    auto functionType = getLLVMType(removeReference(node.getValueType()));
    auto function = llvm::Function::Create(
        static_cast<llvm::FunctionType *>(functionType),
        llvm::Function::ExternalLinkage, node.getName(), &module);
    for (size_t i = 0; i < node.getParameters().size(); i++) {
      const auto &[name, type] = node.getParameters()[i];
      if (type->getTypeClass() == TypeClass::REFERENCE) {
        const auto &referenceType = static_cast<const ReferenceType &>(*type);
        if (referenceType.getConstant()) {
          function->addParamAttr(i,
                                 Attribute::get(context, Attribute::ReadOnly));
        }
      }
    }
    variables.back().insert(
        {node.getName(), {function, removeReference(node.getValueType())}});
    return function;
  }
  ValueType visitTypeAlias(const TypeAliasNode &) override {
    // Do nothing. It is not easy to remove nodes from the AST, so we just
    // ignore them here.
    return nullptr;
  }

private:
  Module &module;
  LLVMContext &context;
  TargetMachine *targetMachine;
  IRBuilder<> irBuilder;
  Function *function = nullptr;
  std::vector<std::map<std::string, VariableInfo>> variables;
  PyriteTypeToLLVMTypeTransformer pyriteToLLVMTypeTransformer;
  Function *globalConstructorFunction;
  IRBuilder<> globalConstructorIrBuilder;

  void createVariableScope() { variables.push_back({}); }
  void destroyVariableScope() {
    if (auto terminator = irBuilder.GetInsertBlock()->getTerminator()) {
      irBuilder.SetInsertPoint(terminator);
    }
    // Inform LLVM that the variables are dead now.
    for (const auto &[name, info] : variables.back()) {
      auto llvmType = getLLVMType(info.type);
      irBuilder.CreateLifetimeEnd(info.value, getSizeOf(llvmType));
    }
    // Go forward to the end of the block (necessary if code depends on us still
    // being at the instruction after the terminator).
    irBuilder.SetInsertPoint(irBuilder.GetInsertBlock());
    variables.pop_back();
  }

  ConstantInt *getSizeOf(llvm::Type *type) {
    // TODO: This is a hack to get around the fact that we don't have a
    // reference to dataLayout in this class.
    return ConstantInt::get(
        llvm::Type::getInt64Ty(context),
        pyriteToLLVMTypeTransformer.dataLayout.getTypeAllocSize(type));
  }

  llvm::Type *getLLVMType(const pyrite::Type &type) {
    return pyriteToLLVMTypeTransformer.visit(type);
  }

  ValueType createTemporaryVariable(llvm::Type *type,
                                    bool mustSurviveToEndOfScope = true) {
    if (!mustSurviveToEndOfScope || function) {
      Function *function = irBuilder.GetInsertBlock()->getParent();
      // Put the alloca in the entry block.
      auto oldIp = irBuilder.saveIP();
      irBuilder.SetInsertPoint(&function->getEntryBlock(),
                               function->getEntryBlock().begin());
      auto temporary = irBuilder.CreateAlloca(type);
      irBuilder.restoreIP(oldIp);
      irBuilder.CreateLifetimeStart(temporary, getSizeOf(type));
      return temporary;
    } else {
      // Put the "temporary" in perminent static storage.
      auto temporary =
          new GlobalVariable(module, type, false, GlobalValue::InternalLinkage,
                             Constant::getNullValue(type), "$temporary");
      irBuilder.CreateLifetimeStart(temporary, getSizeOf(type));
      return temporary;
    }
  }

  void destroyTemporaryVariable(Value *variable) {
    irBuilder.CreateLifetimeEnd(variable, getSizeOf(variable->getType()));
  }
};

void codegen(const AstNode &ast, const std::string &targetTriple,
             const std::string &outputFile, CodeFileType outputFileType,
             OptimizationLevel optimizationLevel) {
  InitializeAllTargets();
  InitializeAllTargetInfos();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
  LLVMContext context;
  Module module(outputFile, context);
  std::string error;
  std::string defaultTargetTriple = sys::getDefaultTargetTriple();
  const std::string *selectedTargetTriple = &targetTriple;
  if (targetTriple.empty()) {
    selectedTargetTriple = &defaultTargetTriple;
  }
  module.setTargetTriple(*selectedTargetTriple);
  auto target = TargetRegistry::lookupTarget(*selectedTargetTriple, error);
  if (!target) {
    throw std::runtime_error(error);
  }
  auto cpu = "generic";
  auto features = "";
  TargetOptions targetOptions;
  auto relocationModel = Optional<Reloc::Model>();
  auto targetMachine = target->createTargetMachine(
      *selectedTargetTriple, cpu, features, targetOptions, relocationModel);
  module.setDataLayout(targetMachine->createDataLayout());
  CodegenAstVisitor codegenVisitor(module, context, targetMachine);
  codegenVisitor.visit(ast);
  codegenVisitor.postCodegen();
  if (verifyModule(module, &errs())) {
    module.print(errs(), nullptr);
    throw std::runtime_error("Module verification failed");
  }
  optimize(module, optimizationLevel);
  std::error_code errorCode;
  raw_fd_ostream dest(outputFile, errorCode, sys::fs::OF_None);
  if (errorCode) {
    throw std::runtime_error("Error: " + errorCode.message());
  }
  legacy::PassManager codegenPassManager;
  if (outputFileType == CodeFileType::ASSEMBLY) {
    targetMachine->addPassesToEmitFile(codegenPassManager, dest, nullptr,
                                       CGFT_AssemblyFile);
  } else if (outputFileType == CodeFileType::OBJECT) {
    targetMachine->addPassesToEmitFile(codegenPassManager, dest, nullptr,
                                       CGFT_ObjectFile);
  } else if (outputFileType == CodeFileType::LLVM_ASSEMBLY) {
    codegenPassManager.add(createPrintModulePass(dest));
  } else if (outputFileType == CodeFileType::LLVM_BITCODE) {
    codegenPassManager.add(createBitcodeWriterPass(dest));
  } else {
    throw std::runtime_error("Error: unsupported output file type");
  }
  codegenPassManager.run(module);
}
} // namespace pyrite
