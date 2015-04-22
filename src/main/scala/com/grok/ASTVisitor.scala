package com.grok

/**
 * Created by brendan.
 */
abstract class ASTVisitor[T, D <: DefinitionTable] extends StatementVisitor[T] {
  def definitionTable: D

  override protected def internalVisitFunctionDefinition(functionDefinition: FunctionDefinition): T = {
    enterScope()
    val returnVal = visitFunctionDefinition(functionDefinition)
    exitScope()
    returnVal
  }

  override protected def internalVisitMethodDefinition(methodDefinition: MethodDefinition): T = {
    enterScope()
    val returnVal = visitMethodDefinition(methodDefinition)
    exitScope()
    returnVal
  }

  override protected def internalVisitBlock(block: Block): T = {
    enterScope()
    val returnVal = visitBlock(block)
    exitScope()
    returnVal
  }

  override protected def internalVisitCase(caseExpression: Case): T = {
    enterScope()
    val returnVal = visitCase(caseExpression)
    exitScope()
    returnVal
  }

  protected def collapseIfExpression(ifExpression: IfExpression): (List[BooleanExpression], List[Expression], Boolean) = {
    val IfExpression(condition, body, alternative) = ifExpression
    alternative match {
      case Some(expression: IfExpression) =>
        val (conditions, bodies, isUnit) = collapseIfExpression(expression)
        (condition +: conditions, body +: bodies, isUnit)
      case Some(expression: Expression) => (List(condition), List(body), true)
      case None => (List(condition), List(body), false)
    }
  }

  protected def enterScope(): Unit = definitionTable.push()

  protected def exitScope(): Unit = definitionTable.pop()
}
