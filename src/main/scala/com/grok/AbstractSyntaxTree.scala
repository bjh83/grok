package com.grok

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
case class ExpressionWrapper(expression: ExpressionStatement) extends Statement
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
abstract class ExpressionStatement extends Expression
// ExpressionStatement ::=
case class IfExpression(condition: BooleanExpression, body: Block, alternative: Option[Expression]) extends ExpressionStatement // I think the alternative works.
case class WhileExpression(condition: BooleanExpression, body: Block) extends ExpressionStatement
case class MatchExpression(expression: Expression, cases: List[Case]) extends ExpressionStatement
case class FunctionCall(identifier: String, parameters: List[Expression]) extends ExpressionStatement
case class MethodCall(reciever: Expression, identifier: String, parameters: List[Expression]) extends ExpressionStatement

// BooleanExpression ::=
case class BooleanBinaryExpression(left: BooleanExpression, operator: BooleanOperator, right: BooleanOperator) extends BooleanExpression
case class BooleanComparison(left: ArithmeticExpression, operator: ComparisonOperator, right: ArithmeticExpression) extends BooleanExpression
case class BooleanFunctionCallWrapper(functionCall: FunctionCall) extends BooleanExpression
case class BooleanMethodCallWrapper(methodCall: MethodCall) extends BooleanExpression
case class BooleanVariableWrapper(variable: Variable) extends BooleanExpression
case class BooleanConstant(value: Boolean) extends BooleanExpression

// ArithmeticExpression ::=
case class ArithmeticBinaryExpression(left: ArithmeticExpression, operator: ArithmeticOperator, right: ArithmeticExpression) extends ArithmeticExpression
case class ArithmeticFunctionCallWrapper(functionCall: FunctionCall) extends ArithmeticExpression
case class ArithmeticMethodCallWrapper(methodCall: MethodCall) extends ArithmeticExpression
case class ArithmeticVariableWrapper(variable: Variable) extends ArithmeticExpression
case class ArithmeticConstant(value: Double) extends ArithmeticExpression

// Others ::=
case class Parameter(identifier: String, paramType: Type)
case class Type(identifier: String)
case class Field(identifier: String, fieldType: Type, isMutable: Boolean)
case class MethodStub(identifier: String, typeParameters: List[Type], parameters: List[Parameter], returnType: Type)