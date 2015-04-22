package com.grok

/**
 * Created by brendan.
 */

sealed abstract class Operand extends ThreeAddressCodeElement
trait TempOperand {
  def id: Int
}
trait RealOperand {
  def name: String
}

trait ConstOperand[T] {
  def value: T
}

sealed abstract class ArithmeticOperand extends Operand
abstract class IntOperand extends ArithmeticOperand
abstract class FloatOperand extends ArithmeticOperand
abstract class BoolOperand extends Operand
abstract class ReferenceOperand extends Operand
case class IntTempOperand(id: Int) extends IntOperand with TempOperand
case class IntRealOperand(name: String) extends IntOperand with RealOperand
case class IntConstOperand(value: Int) extends IntOperand with ConstOperand[Int]
case class FloatTempOperand(id: Int) extends FloatOperand with TempOperand
case class FloatRealOperand(name: String) extends FloatOperand with RealOperand
case class FloatConstOperand(value: Float) extends FloatOperand with ConstOperand[Float]
case class BoolTempOperand(id: Int) extends BoolOperand with TempOperand
case class BoolRealOperand(name: String) extends BoolOperand with RealOperand
case class BoolConstOperand(value: Boolean) extends BoolOperand with ConstOperand[Boolean]
case class ReferenceTempOperand(id: Int) extends ReferenceOperand with TempOperand
case class ReferenceRealOperand(name: String) extends ReferenceOperand with RealOperand

sealed abstract class ThreeAddressCodeElement

abstract class Instruction extends ThreeAddressCodeElement
abstract class Operation[T <: Operand] extends Instruction {
  def result: T
}

// Basic Operations.
abstract class Assign[T <: Operand] extends Operation[T] {
  def result: T
  def value: T
}
abstract class ArithmeticBinaryOperation[T <: ArithmeticOperand] extends Operation[T] {
  def result: T
  def left: T
  def op: ArithmeticOperator
  def right: T
}
abstract class Comparison[T <: ArithmeticOperand] extends Operation[BoolOperand] {
  def result: BoolOperand
  def left: T
  def op: ComparisonOperator
  def right: T
}

case class AssignInt(result: IntOperand, value: IntOperand) extends Assign[IntOperand]
case class BinaryOperationInt(result: IntOperand, left: IntOperand, op: ArithmeticOperator, right: IntOperand) extends ArithmeticBinaryOperation[IntOperand]

case class ToFloat(result: FloatOperand, left: IntOperand) extends Operation[FloatOperand]
case class AssignFloat(result: FloatOperand, value: FloatOperand) extends Assign[FloatOperand]
case class BinaryOperationFloat(result: FloatOperand, left: FloatOperand, op: ArithmeticOperator, right: FloatOperand) extends ArithmeticBinaryOperation[FloatOperand]

case class AssignBool(result: BoolOperand, value: BoolOperand) extends Assign[BoolOperand]
case class BinaryOperationBool(result: BoolOperand, left: BoolOperand, op: BooleanOperator, right: BoolOperand) extends Operation[BoolOperand]
case class Not(result: BoolOperand, left: BoolOperand) extends Operation[BoolOperand]

case class ComparisonInt(result: BoolOperand, left: IntOperand, op: ComparisonOperator, right: IntOperand) extends Comparison[IntOperand]
case class ComparisonFloat(result: BoolOperand, left: FloatOperand, op: ComparisonOperator, right: FloatOperand) extends Comparison[FloatOperand]

case class AssignReference(result: ReferenceOperand, value: ReferenceOperand) extends Assign[ReferenceOperand]

// Branches.
abstract class Label extends ThreeAddressCodeElement
case class Goto(label: Label) extends Instruction
case class ConditionalGoto(condition: BoolOperand, label: Label) extends Instruction
case class ConditionalGotoNot(condition: BoolOperand, label: Label) extends Instruction

// Compound data types.
abstract class CompoundDataType extends ThreeAddressCodeElement
case class Struct(fields: List[Operand]) extends CompoundDataType
case class Union(selector: IntOperand,
                 intValue: IntOperand,
                 floatValue: FloatOperand,
                 boolValue: BoolOperand,
                 referenceValue: ReferenceOperand) extends CompoundDataType

case class CompoundAccessInt(result: IntOperand, reference: ReferenceOperand, offset: Int) extends Operation[IntOperand]
case class CompoundAccessFloat(result: FloatOperand, reference: ReferenceOperand, offset: Int) extends Operation[FloatOperand]
case class CompoundAccessBool(result: BoolOperand, reference: ReferenceOperand, offset: Int) extends Operation[BoolOperand]
case class CompoundAccessReference(result: ReferenceOperand, reference: ReferenceOperand, offset: Int) extends Operation[ReferenceOperand]

case class CompoundAssignInt(value: IntOperand, reference: ReferenceOperand, offset: Int) extends Instruction
case class CompoundAssignFloat(value: FloatOperand, reference: ReferenceOperand, offset: Int) extends Instruction
case class CompoundAssignBool(value: BoolOperand, reference: ReferenceOperand, offset: Int) extends Instruction
case class CompoundAssignReference(value: ReferenceOperand, reference: ReferenceOperand, offset: Int) extends Instruction

case class HeapAllocateStruct(result: ReferenceOperand, fields: List[Operand]) extends Operation[ReferenceOperand]
case class HeapAllocateUnion(result: ReferenceOperand, selector: Int, members: List[Operand]) extends Operation[ReferenceOperand]

// Functions.
case class PushInt(value: IntOperand) extends Instruction
case class PushFloat(value: FloatOperand) extends Instruction
case class PushBool(value: BoolOperand) extends Instruction
case class PushReference(value: ReferenceOperand) extends Instruction

case class Pop(offset: Int) extends Instruction

case class ParamFromStackInt(result: IntOperand, offset: Int) extends Operation[IntOperand]
case class ParamFromStackFloat(result: FloatOperand, offset: Int) extends Operation[FloatOperand]
case class ParamFromStackBool(result: BoolOperand, offset: Int) extends Operation[BoolOperand]
case class ParamFromStackReference(result: ReferenceOperand, offset: Int) extends Operation[ReferenceOperand]

case class CallFuncInt(result: IntOperand, label: Label) extends Operation[IntOperand]
case class CallFuncFloat(result: FloatOperand, label: Label) extends Operation[FloatOperand]
case class CallFuncBool(result: BoolOperand, label: Label) extends Operation[BoolOperand]
case class CallFuncReference(result: ReferenceOperand, label: Label) extends Operation[ReferenceOperand]
case class CallFunc(label: Label) extends Instruction

case class ReturnInt(value: IntOperand) extends Instruction
case class ReturnFloat(value: FloatOperand) extends Instruction
case class ReturnBool(value: BoolOperand) extends Instruction
case class ReturnReference(value: ReferenceOperand) extends Instruction
case object Return extends Instruction