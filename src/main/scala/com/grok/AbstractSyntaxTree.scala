package com.grok

case class CompilationUnit(topLevelStatements: List[TopLevelStatement], symbolTable: SymbolTable)

sealed abstract class TopLevelStatement

// TopLevelStatement ::=
case class FunctionDefinition(identifier: String, typeParameters: List[Type], parameters: List[Parameter], returnType: Type, definition: Expression) extends TopLevelStatement
case class MethodDefinition(receiver: Type, identifier: String, typeParameters: List[Type], parameters: List[Parameter], returnType: Type, definition: Expression) extends TopLevelStatement
case class StructDefinition(identifier: String, typeParameters: List[Type], fields: List[Field]) extends TopLevelStatement
case class UnionDefinition(identifier: String, typeParameters: List[Type], members: List[Type]) extends TopLevelStatement
case class InterfaceDefinition(identifier: String, typeParameters: List[Type], parent: Type, members: List[MethodStub]) extends TopLevelStatement
case class Instance(implementer: Type, interface: Type, typeParameters: List[Type], members: List[MethodDefinition]) extends TopLevelStatement
abstract class Statement extends TopLevelStatement

// Statement ::=
case class VariableDeclaration(mutability: Mutability, identifier: String, varType: Option[Type], value: Expression) extends Statement
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
case class FunctionCall(identifier: String, parameters: List[Expression]) extends Expression
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
case class SimpleType(identifier: String, typeParams: List[Type]) extends Type
case class FunctionType(left: Type, right: Type) extends Type

case class Case(parameter: Parameter, body: Expression)
case class Parameter(identifier: String, paramType: Type)
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