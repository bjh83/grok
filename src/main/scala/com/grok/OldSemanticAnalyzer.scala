package com.grok

/**
 * Created by brendan.
 */
class SemanticAnalyzer {
  private var definitionTable = new DefinitionTable

  def analyze(topLevelStatements: List[TopLevelStatement]): CompilationUnit = {
    definitionTable = new DefinitionTable
    topLevelStatements.foreach(visitTopLevelStatement)
    CompilationUnit(topLevelStatements, definitionTable)
  }

  private val visitTopLevelStatement: PartialFunction[TopLevelStatement, Unit] = {
    case functionDefinition: FunctionDefinition => visitFunctionDefinition(functionDefinition)
    case methodDefinition: MethodDefinition => sys.error("Not yet implemented")
    case structDefinition: StructDefinition => definitionTable.addDefinition(structDefinition)
    case unionDefinition: UnionDefinition => definitionTable.addDefinition(unionDefinition)
    case interfaceDefinition: InterfaceDefinition => definitionTable.addDefinition(interfaceDefinition)
    case instance: Instance => sys.error("Not yet implemented")
    case statement: Statement => visitStatement(statement)
  }

  private def visitFunctionDefinition(functionDefinition: FunctionDefinition): Unit = {
    val FunctionDefinition(_, _, parameters, _, definition) = functionDefinition
    definitionTable.addSymbol(functionDefinition)
    definitionTable.push()
    parameters.foreach(definitionTable.addSymbol)
    visitExpression(definition)
    definitionTable.pop()
  }

  private val visitStatement: PartialFunction[Statement, Unit] = {
    case variableDeclaration: VariableDeclaration => definitionTable.addSymbol(variableDeclaration)
    case variableAssignment: VariableAssignment => definitionTable.containsSymbolFail(VariableKey(variableAssignment.identifier))
    case structAssignment: StructAssignment => definitionTable.containsSymbolFail(VariableKey(structAssignment.identifier))
    case ExpressionWrapper(expr) => visitExpression(expr)
  }

  private val visitExpression: PartialFunction[Expression, Unit] = {
    case block: Block => visitBlock(block)
    case lambda: Lambda =>
    case booleanExpression: BooleanExpression => visitBooleanExpression(booleanExpression)
    case arithmeticExpression: ArithmeticExpression => visitArithmeticExpression(arithmeticExpression)
    case Variable(variable) => definitionTable.containsSymbolFail(VariableKey(variable))
    case IfExpression(condition, body, alternative) => visitIfExpression(condition, body, alternative)
    case WhileExpression(condition, body) => visitWhileExpression(condition, body)
    case MatchExpression(expression, cases) => visitMatchExpression(expression, cases)
    case FunctionCall(identifier, params) => visitFunctionCall(identifier, params)
    case MethodCall(receiver, identifier, params) => sys.error("Not yet implemented")
    case StructAccess(receiver, identifier) => visitStructAccess(receiver, identifier)
    case This => sys.error("Not yet implemented")
  }

  private def visitBlock(block: Block): Unit = {
    definitionTable.push()
    block.statements.foreach(visitStatement)
    block.expression.foreach(visitExpression)
    definitionTable.pop()
  }

  private val visitBooleanExpression: PartialFunction[BooleanExpression, Unit] = {
    case BooleanBinaryExpression(left, _, right) => visitBooleanExpression(left); visitBooleanExpression(right)
    case BooleanInverse(value) => visitBooleanExpression(value)
    case BooleanComparison(left, _, right) => visitArithmeticExpression(left); visitArithmeticExpression(right)
    case BooleanExpressionWrapper(value) => visitExpression(value)
    case _: BooleanConstant =>
  }

  private val visitArithmeticExpression: PartialFunction[ArithmeticExpression, Unit] = {
    case ArithmeticBinaryExpression(left, _, right) => visitArithmeticExpression(left); visitArithmeticExpression(right)
    case ArithmeticExpressionWrapper(expr) => visitExpression(expr)
    case ArithmeticIntegralConstant(_) =>
    case ArithmeticFloatingPointConstant(_) =>
  }

  private def visitIfExpression(condition: BooleanExpression, body: Block, alternative: Option[Expression]): Unit = {
    visitBooleanExpression(condition)
    visitBlock(body)
    alternative.foreach(visitExpression)
  }

  private def visitWhileExpression(condition: BooleanExpression, body: Block): Unit = {
    visitBooleanExpression(condition)
    visitBlock(body)
  }

  private def visitMatchExpression(expression: Expression, cases: List[Case]): Unit = {
    visitExpression(expression)
    cases.foreach { case Case(param, expr) =>
      definitionTable.push()
      definitionTable.addSymbol(param)
      visitExpression(expr)
      definitionTable.pop()
    }
  }

  private def visitFunctionCall(identifier: String, params: List[Expression]): Unit = {
    definitionTable.containsSymbolFail(VariableKey(identifier))
    params.foreach(visitExpression)
  }

  private def visitStructAccess(receiver: Expression, member: String): Unit = {
    visitExpression(receiver)
  }
}