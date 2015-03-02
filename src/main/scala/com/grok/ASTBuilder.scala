package com.grok

import scala.collection.JavaConverters._
import com.grok.GrokParser._
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor

class ASTBuilder extends GrokBaseVisitor[List[TopLevelStatement]] {
  override def visitExpression(ctx: ExpressionContext): Result = ???

  override def visitTypeParameters(ctx: TypeParametersContext): Result = ???

  override def visitMatchCase(ctx: MatchCaseContext): Result = ???

  override def visitLambdaParameter(ctx: LambdaParameterContext): Result = ???

  override def visitEos(ctx: EosContext): Result = ???

  override def visitArguments(ctx: ArgumentsContext): Result = ???

  override def visitArgument(ctx: ArgumentContext): Result = ???

  override def visitLambdaParameters(ctx: LambdaParametersContext): Result = ???

  override def visitField(ctx: FieldContext): Result = ???

  override def visitMethodStub(ctx: MethodStubContext): Result = ???

  override def visitFuncParameter(ctx: FuncParameterContext): Result = ???

  override def visitInstanceMethod(ctx: InstanceMethodContext): Result = ???

  override def visitInnerStatement(ctx: InnerStatementContext): Result = ???

  override def visitCompilationUnit(ctx: CompilationUnitContext): List[TopLevelStatement] = {
    val visitor = new TopLevelStatementVisitor
    ctx.topLevelStatement().asScala.map { topLevelStmt => visitor.visit(topLevelStmt) }
  }

  override def visitType(ctx: TypeContext): Result = ???

  override def visitFuncParameters(ctx: FuncParametersContext): Result = ???

  override def visitTopLevelStatement(ctx: TopLevelStatementContext): Result = ???
}

class TopLevelStatementVisitor extends GrokBaseVisitor[TopLevelStatement] {
  override def visitFunctionDefinition(ctx: FunctionDefinitionContext): TopLevelStatement = {
    ctx.typeParameters()
    FunctionDefinition()
  }

  override def visitMethodDefintion(ctx: MethodDefintionContext): TopLevelStatement = ???
  override def visitStructDefinition(ctx: StructDefinitionContext): TopLevelStatement = ???
  override def visitUnionDefintion(ctx: UnionDefintionContext): TopLevelStatement = ???
  override def visitInterfaceDefinition(ctx: InterfaceDefinitionContext): TopLevelStatement = ???
  override def visitInstance(ctx: InstanceContext): TopLevelStatement = ???
  override def visitStatement(ctx: StatementContext): TopLevelStatement = ???
}

class StatementVisitor extends GrokBaseVisitor[Statement] {
  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Statement = ???
  override def visitVariableAssignment(ctx: VariableAssignmentContext): Statement = ???
  override def visitStructAssignment(ctx: StructAssignmentContext): Statement = ???
  override def visitIfExpression(ctx: IfExpressionContext): Statement = ???
  override def visitWhileExpression(ctx: WhileExpressionContext): Statement = ???
  override def visitMatchExpression(ctx: MatchExpressionContext): Statement = ???
  override def visitFunctionCall(ctx: FunctionCallContext): Statement = ???
}

class ExpressionVisitor extends GrokBaseVisitor[Expression] {
  override def visitIfExpression(ctx: IfExpressionContext): Expression = ???
  override def visitWhileExpression(ctx: WhileExpressionContext): Expression = ???
  override def visitMatchExpression(ctx: MatchExpressionContext): Expression = ???
  override def visitFunctionCall(ctx: FunctionCallContext): Expression = ???
  override def visitBlock(ctx: BlockContext): Expression = ???
  override def visitLambda(ctx: LambdaContext): Expression = ???
  override def visitBooleanExpression(ctx: BooleanExpressionContext): Expression = ???
  override def visitArithmeticExpression(ctx: ArithmeticExpressionContext): Expression = ???
  override def visitVariable(ctx: VariableContext): Expression = ???
}

class BooleanExpressionVisitor extends GrokBaseVisitor[BooleanExpression] {
  override def visitBooleanExpression(ctx: BooleanExpressionContext): BooleanExpression = ???
  override def visitBooleanProduct(ctx: BooleanProductContext): BooleanExpression = ???
  override def visitBooleanInverse(ctx: BooleanInverseContext): BooleanExpression = ???
  override def visitBooleanTerm(ctx: BooleanTermContext): BooleanExpression = ???
  override def visitComparison(ctx: ComparisonContext): BooleanExpression = ???
}

class ArithmeticExpressionVisitor extends GrokBaseVisitor[ArithmeticExpression] {
  override def visitArithmeticExpression(ctx: ArithmeticExpressionContext): ArithmeticExpression = ???
  override def visitArithmeticProduct(ctx: ArithmeticProductContext): ArithmeticExpression = ???
  override def visitArithmeticTerm(ctx: ArithmeticTermContext): ArithmeticExpression = ???
}

class IdentifierVisitor extends GrokBaseVisitor[String] {
  override def visitIdentifier(ctx: IdentifierContext): String = ???
}