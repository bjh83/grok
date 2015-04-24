package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
class VirtualMachine(private val code: List[Instruction]) {
  private var programCounter = 0
  private var returnOperand: Operand = _
  private val programCounterStack = mutable.Stack[Int]()
  private val stack = mutable.Stack[mutable.Map[Operand, OperandValue[_]]]()
  private val heap = mutable.MutableList[OperandValue[_]]()
  private val paramStack = mutable.MutableList[OperandValue[_]]()

  private def evaluate[T](operand: Operand): T = {
    operand match {
      case op: ConstOperand[T] => op.value
      case op =>
        stack.head(op).asInstanceOf[OperandValue[T]].value
    }
  }

  def execute(): Unit = {
    executeInstruction(code(0))
    while (stack.nonEmpty) {
      executeInstruction(code(programCounter))
    }
  }

  private def executeInstruction(instruction: Instruction): Unit = {
    instruction match {
      case operation: Operation[_] => executeOperation(operation)
      case Goto(label) => programCounter = label.toRealizedLabel.index
      case ConditionalGoto(condition, label) =>
        if (evaluate(condition)) {
          programCounter = label.toRealizedLabel .index
        } else {
          programCounter += 1
        }
      case ConditionalGotoNot(condition, label) =>
        if (!evaluate[Boolean](condition)) {
          programCounter = label.toRealizedLabel .index
        } else {
          programCounter += 1
        }
      case CompoundAssignInt(value, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        heap(heapAddress) = IntOperandValue(evaluate(value))
        programCounter += 1
      case CompoundAssignFloat(value, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        heap(heapAddress) = FloatOperandValue(evaluate(value))
        programCounter += 1
      case CompoundAssignBool(value, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        heap(heapAddress) = BoolOperandValue(evaluate(value))
        programCounter += 1
      case CompoundAssignReference(value, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        heap(heapAddress) = stack.head(value)
        programCounter += 1
      case PushInt(value) =>
        paramStack += IntOperandValue(evaluate(value))
        programCounter += 1
      case PushFloat(value) =>
        paramStack += FloatOperandValue(evaluate(value))
        programCounter += 1
      case PushBool(value) =>
        paramStack += BoolOperandValue(evaluate(value))
        programCounter += 1
      case PushReference(value) =>
        paramStack += stack.head(value)
        programCounter += 1
      case Pop(size) =>
        paramStack.dropRight(size)
        programCounter += 1
      case CallFunc(label) => callFunc(label)
      case ReturnInt(value) =>
        val result = IntOperandValue(evaluate(value))
        stack.pop()
        programCounter = programCounterStack.pop()
        stack.head(returnOperand) = result
      case ReturnFloat(value) =>
        val result = FloatOperandValue(evaluate(value))
        stack.pop()
        programCounter = programCounterStack.pop()
        stack.head(returnOperand) = result
      case ReturnBool(value) =>
        val result = BoolOperandValue(evaluate(value))
        stack.pop()
        programCounter = programCounterStack.pop()
        stack.head(returnOperand) = result
      case ReturnReference(value) =>
        val result = stack.head(value)
        stack.pop()
        programCounter = programCounterStack.pop()
        stack.head(returnOperand) = result
      case Return =>
        stack.pop()
        programCounter = programCounterStack.pop()
      case Print(value) =>
        println(evaluate(value))
        programCounter += 1
    }
  }

  private def executeOperation(operation: Operation[_]): Unit = {
    operation match {
      case AssignInt(result, value) =>
        stack.head(result) = IntOperandValue(evaluate(value))
        programCounter += 1
      case BinaryOperationInt(result, left, op, right) =>
        val leftInt = evaluate[Int](left)
        val rightInt = evaluate[Int](right)
        stack.head(result) = IntOperandValue(op match {
          case PLUS => leftInt + rightInt
          case MINUS => leftInt - rightInt
          case MULTIPLY => leftInt * rightInt
          case DIVIDE => leftInt / rightInt
          case MODULUS => leftInt % rightInt
        })
        programCounter += 1
      case ToFloat(result, value) =>
        stack.head(result) = FloatOperandValue(evaluate[Int](value))
        programCounter += 1
      case AssignFloat(result, value) =>
        stack.head(result) = FloatOperandValue(evaluate(value))
        programCounter += 1
      case BinaryOperationFloat(result, left, op, right) =>
        val leftFloat = evaluate[Float](left)
        val rightFloat = evaluate[Float](right)
        stack.head(result) = FloatOperandValue(op match {
          case PLUS => leftFloat + rightFloat
          case MINUS => leftFloat - rightFloat
          case MULTIPLY => leftFloat * rightFloat
          case DIVIDE => leftFloat / rightFloat
          case MODULUS => leftFloat % rightFloat
        })
        programCounter += 1
      case AssignBool(result, value) =>
        stack.head(result) = BoolOperandValue(evaluate(value))
        programCounter += 1
      case BinaryOperationBool(result, left, op, right) =>
        val leftBool = evaluate[Boolean](left)
        val rightBool = evaluate[Boolean](right)
        stack.head(result) = BoolOperandValue(op match {
          case AND => leftBool && rightBool
          case OR => leftBool || rightBool
        })
        programCounter += 1
      case Not(result, value) =>
        stack.head(result) = BoolOperandValue(!evaluate[Boolean](value))
        programCounter += 1
      case ComparisonInt(result, left, op, right) =>
        val leftInt = evaluate[Int](left)
        val rightInt = evaluate[Int](right)
        stack.head(result) = BoolOperandValue(op match {
          case EQUALS => leftInt == rightInt
          case NOT_EQUALS => leftInt != rightInt
          case LESS => leftInt < rightInt
          case GREATER => leftInt > rightInt
          case LESS_OR_EQUAL => leftInt <= rightInt
          case GREATER_OR_EQUAL => leftInt >= rightInt
        })
        programCounter += 1
      case ComparisonFloat(result, left, op, right) =>
        val leftFloat = evaluate[Float](left)
        val rightFloat = evaluate[Float](right)
        stack.head(result) = BoolOperandValue(op match {
          case EQUALS => leftFloat == rightFloat
          case NOT_EQUALS => leftFloat != rightFloat
          case LESS => leftFloat < rightFloat
          case GREATER => leftFloat > rightFloat
          case LESS_OR_EQUAL => leftFloat <= rightFloat
          case GREATER_OR_EQUAL => leftFloat >= rightFloat
        })
        programCounter += 1
      case AssignReference(result, value) =>
        stack.head(result) = stack.head(value)
        programCounter += 1
      case CompoundAccessInt(result, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        stack.head(result) = heap(heapAddress)
        programCounter += 1
      case CompoundAccessFloat(result, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        stack.head(result) = heap(heapAddress)
        programCounter += 1
      case CompoundAccessBool(result, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        stack.head(result) = heap(heapAddress)
        programCounter += 1
      case CompoundAccessReference(result, reference, offset) =>
        val heapAddress = stack.head(reference).asInstanceOf[ReferenceOperandValue].value + offset
        stack.head(result) = heap(heapAddress)
        programCounter += 1
      case operation: HeapAllocateStruct =>
        allocateStruct(operation)
        programCounter += 1
      case operation: HeapAllocateUnion =>
        allocateUnion(operation)
        programCounter += 1
      case ParamFromStackInt(result, offset) =>
        stack.head(result) = paramStack(paramStack.size - 1 - offset)
        programCounter += 1
      case ParamFromStackFloat(result, offset) =>
        stack.head(result) = paramStack(paramStack.size - 1 - offset)
        programCounter += 1
      case ParamFromStackBool(result, offset) =>
        stack.head(result) = paramStack(paramStack.size - 1 - offset)
        programCounter += 1
      case ParamFromStackReference(result, offset) =>
        stack.head(result) = paramStack(paramStack.size - 1 - offset)
        programCounter += 1
      case CallFuncInt(result, label) =>
        returnOperand = result
        callFunc(label)
      case CallFuncFloat(result, label) =>
        returnOperand = result
        callFunc(label)
      case CallFuncBool(result, label) =>
        returnOperand = result
        callFunc(label)
      case CallFuncReference(result, label) =>
        returnOperand = result
        callFunc(label)
    }
  }

  private def allocateStruct(operation: HeapAllocateStruct): Unit = {
    val result = operation.result
    val fields = operation.fields.map {
      case _: IntOperand => IntOperandValue(0)
      case _: FloatOperand => FloatOperandValue(0)
      case _: BoolOperand => BoolOperandValue(false)
      case _: ReferenceOperand => ReferenceOperandValue(-1)
    }
    val startAddress = heap.size
    heap ++= fields
    stack.head(result) = ReferenceOperandValue(startAddress)
  }

  private def allocateUnion(operation: HeapAllocateUnion): Unit = {
    val result = operation.result
    val fields = IntOperandValue(operation.selector) +: operation.members.map {
      case _: IntOperand => IntOperandValue(0)
      case _: FloatOperand => FloatOperandValue(0)
      case _: BoolOperand => BoolOperandValue(false)
      case _: ReferenceOperand => ReferenceOperandValue(-1)
    }
    val startAddress = heap.size
    heap ++= fields
    stack.head(result) = ReferenceOperandValue(startAddress)
  }

  private def callFunc(label: Label): Unit = {
    stack.push(mutable.Map())
    programCounterStack.push(programCounter + 1)
    programCounter = label.toRealizedLabel.index
  }
}

sealed abstract class OperandValue[+T] {
  def value: T
}
case class IntOperandValue(value: Int) extends OperandValue[Int]
case class FloatOperandValue(value: Float) extends OperandValue[Float]
case class BoolOperandValue(value: Boolean) extends OperandValue[Boolean]
case class ReferenceOperandValue(value: Int) extends OperandValue[Int]