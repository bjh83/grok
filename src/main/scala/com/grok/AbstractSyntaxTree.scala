package com.grok

case class CompilationUnit(topLevelStatements: List[TopLevelStatement], definitionTable: DefinitionTable)

trait TypeDefinition {
  def identifier: String
  def key(): Key
  def definesType(): Type
}

trait Symbol {
  def identifier: String
  def key(): Key
  def `type`(): Type
}

sealed abstract class TopLevelStatement

// TopLevelStatement ::=
case class FunctionDefinition(identifier: String,
                              typeParameters: List[Type],
                              parameters: List[Parameter],
                              returnType: Type,
                              definition: Expression) extends TopLevelStatement with Symbol {
  def key(): FullFunctionKey = FullFunctionKey(identifier, returnType, parameters.map(_.paramType))
  def `type`() = FunctionType(parameters.map(_.`type`().asInstanceOf[NonFunctionType]))
}

case class MethodDefinition(receiver: Type, identifier: String, typeParameters: List[Type], parameters: List[Parameter], returnType: Type, definition: Expression) extends TopLevelStatement
case class StructDefinition(identifier: String,
                            typeParameters: List[Type],
                            fields: List[Field]) extends TopLevelStatement with TypeDefinition {
  def key(): DefinitionKey = DefinitionKey(identifier)
  def definesType() = SimpleType(identifier, typeParameters)
}

case class UnionDefinition(identifier: String,
                           typeParameters: List[Type],
                           members: List[Type]) extends TopLevelStatement with TypeDefinition{
  def key(): DefinitionKey = DefinitionKey(identifier)
  def definesType() = SimpleType(identifier, typeParameters)
}

case class InterfaceDefinition(identifier: String,
                               typeParameters: List[Type],
                               parent: Type,
                               members: List[MethodStub]) extends TopLevelStatement with TypeDefinition {
  def key(): DefinitionKey = DefinitionKey(identifier)
  def definesType() = SimpleType(identifier, typeParameters)
}

case class Instance(implementer: Type, interface: Type, typeParameters: List[Type], members: List[MethodDefinition]) extends TopLevelStatement
abstract class Statement extends TopLevelStatement

// Statement ::=
case class VariableDeclaration(mutability: Mutability,
                               identifier: String,
                               varType: Option[Type],
                               value: Expression) extends Statement with Symbol {
  def key(): VariableKey = VariableKey(identifier)
  def `type`() = varType.getOrElse(sys.error("Was not able to get type for: " + identifier))
}

case class VariableAssignment(identifier: String, value: Expression) extends Statement
case class StructAssignment(identifier: String, member: String, value: Expression) extends Statement
case class ExpressionWrapper(expression: Expression) extends Statement
// Included from ExpressionWrapper:
// case class IfExpression
// case class WhileExpression
// case class MatchExpression
// case class FunctionCall
// case class MethodCall

sealed abstract class Expression
// Expression ::=
case class Block(statements: List[Statement], expression: Option[Expression]) extends Expression
case class Lambda(parameters: List[ParameterOptionalType], expression: Expression) extends Expression
abstract class BooleanExpression extends Expression
abstract class ArithmeticExpression extends Expression
case class Variable(identifier: String) extends Expression
case class IfExpression(condition: BooleanExpression, body: Block, alternative: Option[Expression]) extends Expression // I think the alternative works.
case class WhileExpression(condition: BooleanExpression, body: Block) extends Expression
case class MatchExpression(expression: Expression, cases: List[Case]) extends Expression
case class FunctionCall(identifier: String, parameters: List[Expression]) extends Expression {
  var definition: FunctionDefinition = null
}
case class MethodCall(receiver: Expression, identifier: String, parameters: List[Expression]) extends Expression
case class StructAccess(receiver: Expression, identifier: String) extends Expression
case object This extends Expression

// BooleanExpression ::=
case class BooleanBinaryExpression(left: BooleanExpression, operator: BooleanOperator, right: BooleanExpression) extends BooleanExpression
case class BooleanInverse(value: BooleanExpression) extends BooleanExpression
case class BooleanComparison(left: ArithmeticExpression, operator: ComparisonOperator, right: ArithmeticExpression) extends BooleanExpression
case class BooleanExpressionWrapper(expression: Expression) extends BooleanExpression
case class BooleanConstant(value: Boolean) extends BooleanExpression

// ArithmeticExpression ::=
case class ArithmeticBinaryExpression(left: ArithmeticExpression, operator: ArithmeticOperator, right: ArithmeticExpression) extends ArithmeticExpression
case class ArithmeticExpressionWrapper(expression: Expression) extends ArithmeticExpression
case class ArithmeticIntegralConstant(value: Int) extends ArithmeticExpression
case class ArithmeticFloatingPointConstant(value: Double) extends ArithmeticExpression

// Others ::=
sealed abstract class Type
abstract class NonFunctionType extends Type
case class SimpleType(identifier: String, typeParams: List[Type]) extends NonFunctionType
case class FunctionType(left: NonFunctionType, right: Type) extends Type {
  def toList: List[NonFunctionType] = right match {
    case functionType: FunctionType => left +: functionType.toList
    case nonFunctionType: NonFunctionType => List(left, nonFunctionType)
  }
}

object FunctionType {
  def apply(list: List[NonFunctionType]): FunctionType = list match {
    case first :: second :: List() => FunctionType(first, second)
    case first :: rest => FunctionType(first, apply(rest))
  }
}
case object BoolType extends NonFunctionType
abstract class ArithmeticType extends NonFunctionType
case object IntegralType extends ArithmeticType
case object FloatingPointType extends ArithmeticType
case object UnitType extends NonFunctionType

case class Case(parameter: Parameter, body: Expression)
case class Parameter(identifier: String, paramType: Type) extends Symbol {
  def key() = VariableKey(identifier)
  def `type`() = paramType
}
case class ParameterOptionalType(identifier: String, paramType: Option[Type])
case class Field(identifier: String, fieldType: Type, mutability: Mutability)
case class MethodStub(identifier: String, typeParameters: List[Type], parameters: List[Parameter], returnType: Type)

sealed abstract class Mutability
case object IMMUTABLE extends Mutability
case object MUTABLE extends Mutability

sealed abstract class BooleanOperator
case object OR extends BooleanOperator
case object AND extends BooleanOperator

sealed abstract class ComparisonOperator
case object EQUALS extends ComparisonOperator
case object NOT_EQUALS extends ComparisonOperator
case object LESS extends ComparisonOperator
case object GREATER extends ComparisonOperator
case object LESS_OR_EQUAL extends ComparisonOperator
case object GREATER_OR_EQUAL extends ComparisonOperator

sealed abstract class ArithmeticOperator
case object PLUS extends ArithmeticOperator
case object MINUS extends ArithmeticOperator
case object MULTIPLY extends ArithmeticOperator
case object DIVIDE extends ArithmeticOperator
case object MODULUS extends ArithmeticOperator