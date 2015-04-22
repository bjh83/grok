package com.grok

/**
 * Created by brendan.
 */
abstract class StatementVisitor[T] {
  protected val visitTopLevelStatement: PartialFunction[TopLevelStatement, T] = {
    case functionDefinition: FunctionDefinition => internalVisitFunctionDefinition(functionDefinition)
    case methodDefinition: MethodDefinition => internalVisitMethodDefinition(methodDefinition)
    case structDefinition: StructDefinition => internalVisitStructDefinition(structDefinition)
    case unionDefinition: UnionDefinition => internalVisitUnionDefinition(unionDefinition)
    case interfaceDefinition: InterfaceDefinition => internalVisitInterfaceDefinition(interfaceDefinition)
    case instance: Instance => internalVisitInstance(instance)
    case statement: Statement => internalVisitStatement(statement)
  }

  protected val visitStatement: PartialFunction[Statement, T] = {
    case variableDeclaration: VariableDeclaration => internalVisitVariableDeclaration(variableDeclaration)
    case variableAssignment: VariableAssignment => internalVisitVariableAssignment(variableAssignment)
    case structAssignment: StructAssignment => internalVisitStructAssignment(structAssignment)
    case ExpressionWrapper(expr) => internalVisitExpression(expr)
  }

  protected val visitExpression: PartialFunction[Expression, T] = {
    case block: Block => internalVisitBlock(block)
    case lambda: Lambda => internalVisitLambda(lambda)
    case booleanExpression: BooleanExpression => internalVisitBooleanExpression(booleanExpression)
    case arithmeticExpression: ArithmeticExpression => internalVisitArithmeticExpression(arithmeticExpression)
    case variable: Variable => internalVisitVariable(variable)
    case ifExpression: IfExpression => internalVisitIfExpression(ifExpression)
    case whileExpression: WhileExpression => internalVisitWhileExpression(whileExpression)
    case matchExpression: MatchExpression => internalVisitMatchExpression(matchExpression)
    case functionCall: FunctionCall => internalVisitFunctionCall(functionCall)
    case methodCall: MethodCall => internalVisitMethodCall(methodCall)
    case structAccess: StructAccess => internalVisitStructAccess(structAccess)
    case This => internalVisitThis()
  }

  protected val visitBooleanExpression: PartialFunction[BooleanExpression, T] = {
    case booleanBinaryExpression: BooleanBinaryExpression => internalVisitBooleanBinaryExpression(booleanBinaryExpression)
    case booleanInverse: BooleanInverse => internalVisitBooleanInverse(booleanInverse)
    case booleanComparison: BooleanComparison => internalVisitBooleanComparison(booleanComparison)
    case booleanExpressionWrapper: BooleanExpressionWrapper => internalVisitBooleanExpressionWrapper(booleanExpressionWrapper)
    case booleanConstant: BooleanConstant => internalVisitBooleanConstant(booleanConstant)
  }

  protected val visitArithmeticExpression: PartialFunction[ArithmeticExpression, T] = {
    case arithmeticBinaryExpression: ArithmeticBinaryExpression => internalVisitArithmeticBinaryExpression(arithmeticBinaryExpression)
    case arithmeticExpressionWrapper: ArithmeticExpressionWrapper => internalVisitArithmeticExpressionWrapper(arithmeticExpressionWrapper)
    case arithmeticIntegralConstant: ArithmeticIntegralConstant => internalVisitArithmeticIntegralConstant(arithmeticIntegralConstant)
    case arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant => internalVisitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant)
  }

  protected def internalVisitTopLevelStatement(topLevelStatement: TopLevelStatement): T = {
    visitTopLevelStatement(topLevelStatement)
  }

  protected def internalVisitStatement(statement: Statement): T = {
    visitStatement(statement)
  }

  protected def internalVisitExpression(expression: Expression): T = {
    visitExpression(expression)
  }

  protected def internalVisitBooleanExpression(booleanExpression: BooleanExpression): T = {
    visitBooleanExpression(booleanExpression)
  }

  protected def internalVisitArithmeticExpression(arithmeticExpression: ArithmeticExpression): T = {
    visitArithmeticExpression(arithmeticExpression)
  }

  protected def internalVisitFunctionDefinition(functionDefinition: FunctionDefinition): T = {
    visitFunctionDefinition(functionDefinition)
  }

  protected def internalVisitMethodDefinition(methodDefinition: MethodDefinition): T = {
    visitMethodDefinition(methodDefinition)
  }

  protected def internalVisitStructDefinition(structDefinition: StructDefinition): T = {
    visitStructDefinition(structDefinition)
  }

  protected def internalVisitUnionDefinition(unionDefinition: UnionDefinition): T = {
    visitUnionDefinition(unionDefinition)
  }

  protected def internalVisitInterfaceDefinition(interfaceDefinition: InterfaceDefinition): T = {
    visitInterfaceDefinition(interfaceDefinition)
  }

  protected def internalVisitInstance(instance: Instance): T = {
    visitInstance(instance)
  }

  protected def internalVisitVariableDeclaration(variableDeclaration: VariableDeclaration): T = {
    visitVariableDeclaration(variableDeclaration)
  }

  protected def internalVisitVariableAssignment(variableAssignment: VariableAssignment): T = {
    visitVariableAssignment(variableAssignment)
  }

  protected def internalVisitStructAssignment(structAssignment: StructAssignment): T = {
    visitStructAssignment(structAssignment)
  }

  protected def internalVisitBlock(block: Block): T = {
    visitBlock(block)
  }

  protected def internalVisitLambda(lambda: Lambda): T = {
    visitLambda(lambda)
  }

  protected def internalVisitVariable(variable: Variable): T = {
    visitVariable(variable)
  }

  protected def internalVisitIfExpression(ifExpression: IfExpression): T = {
    visitIfExpression(ifExpression)
  }

  protected def internalVisitWhileExpression(whileExpression: WhileExpression): T = {
    visitWhileExpression(whileExpression)
  }

  protected def internalVisitMatchExpression(matchExpression: MatchExpression): T = {
    visitMatchExpression(matchExpression)
  }

  protected def internalVisitCase(caseExpression: Case): T = {
    visitCase(caseExpression)
  }

  protected def internalVisitFunctionCall(functionCall: FunctionCall): T = {
    visitFunctionCall(functionCall)
  }

  protected def internalVisitMethodCall(methodCall: MethodCall): T = {
    visitMethodCall(methodCall)
  }

  protected def internalVisitStructAccess(structAccess: StructAccess): T = {
    visitStructAccess(structAccess)
  }

  protected def internalVisitThis(): T = {
    visitThis()
  }

  protected def internalVisitBooleanBinaryExpression(booleanBinaryExpression: BooleanBinaryExpression): T = {
    visitBooleanBinaryExpression(booleanBinaryExpression)
  }

  protected def internalVisitBooleanInverse(booleanInverse: BooleanInverse): T = {
    visitBooleanInverse(booleanInverse)
  }

  protected def internalVisitBooleanComparison(booleanComparison: BooleanComparison): T = {
    visitBooleanComparison(booleanComparison)
  }

  protected def internalVisitBooleanExpressionWrapper(booleanExpressionWrapper: BooleanExpressionWrapper): T = {
    visitBooleanExpressionWrapper(booleanExpressionWrapper)
  }

  protected def internalVisitBooleanConstant(booleanConstant: BooleanConstant): T = {
    visitBooleanConstant(booleanConstant)
  }

  protected def internalVisitArithmeticBinaryExpression(arithmeticBinaryExpression: ArithmeticBinaryExpression): T = {
    visitArithmeticBinaryExpression(arithmeticBinaryExpression)
  }

  protected def internalVisitArithmeticExpressionWrapper(arithmeticExpressionWrapper: ArithmeticExpressionWrapper): T = {
    visitArithmeticExpressionWrapper(arithmeticExpressionWrapper)
  }

  protected def internalVisitArithmeticIntegralConstant(arithmeticIntegralConstant: ArithmeticIntegralConstant): T = {
    visitArithmeticIntegralConstant(arithmeticIntegralConstant)
  }

  protected def internalVisitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant): T = {
    visitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant)
  }

  protected def visitFunctionDefinition(functionDefinition: FunctionDefinition): T

  protected def visitMethodDefinition(methodDefinition: MethodDefinition): T

  protected def visitStructDefinition(structDefinition: StructDefinition): T

  protected def visitUnionDefinition(unionDefinition: UnionDefinition): T

  protected def visitInterfaceDefinition(interfaceDefinition: InterfaceDefinition): T

  protected def visitInstance(instance: Instance): T

  protected def visitVariableDeclaration(variableDeclaration: VariableDeclaration): T

  protected def visitVariableAssignment(variableAssignment: VariableAssignment): T

  protected def visitStructAssignment(structAssignment: StructAssignment): T

  protected def visitBlock(block: Block): T

  protected def visitLambda(lambda: Lambda): T

  protected def visitVariable(variable: Variable): T

  protected def visitIfExpression(ifExpression: IfExpression): T

  protected def visitWhileExpression(whileExpression: WhileExpression): T

  protected def visitMatchExpression(matchExpression: MatchExpression): T

  protected def visitCase(caseExpression: Case): T

  protected def visitFunctionCall(functionCall: FunctionCall): T

  protected def visitMethodCall(methodCall: MethodCall): T

  protected def visitStructAccess(structAccess: StructAccess): T

  protected def visitThis(): T

  protected def visitBooleanBinaryExpression(booleanBinaryExpression: BooleanBinaryExpression): T

  protected def visitBooleanInverse(booleanInverse: BooleanInverse): T

  protected def visitBooleanComparison(booleanComparison: BooleanComparison): T

  protected def visitBooleanExpressionWrapper(booleanExpressionWrapper: BooleanExpressionWrapper): T

  protected def visitBooleanConstant(booleanConstant: BooleanConstant): T

  protected def visitArithmeticBinaryExpression(arithmeticBinaryExpression: ArithmeticBinaryExpression): T

  protected def visitArithmeticExpressionWrapper(arithmeticExpressionWrapper: ArithmeticExpressionWrapper): T

  protected def visitArithmeticIntegralConstant(arithmeticIntegralConstant: ArithmeticIntegralConstant): T

  protected def visitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant): T
}