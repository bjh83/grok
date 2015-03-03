package com.grok

import scala.collection.JavaConverters._
import com.grok.GrokParser._
import com.grok.Utilities._
import org.antlr.v4.runtime.tree.{TerminalNode, ParseTree, AbstractParseTreeVisitor}

class ASTBuilder extends GrokBaseVisitor[List[TopLevelStatement]] {
  override def visitMatchCase(ctx: MatchCaseContext): Result = ???

  override def visitLambdaParameter(ctx: LambdaParameterContext): Result = ???

  override def visitEos(ctx: EosContext): Result = ???

  override def visitArguments(ctx: ArgumentsContext): Result = ???

  override def visitArgument(ctx: ArgumentContext): Result = ???

  override def visitLambdaParameters(ctx: LambdaParametersContext): Result = ???

  override def visitFuncParameter(ctx: FuncParameterContext): Result = ???

  override def visitInnerStatement(ctx: InnerStatementContext): Result = ???

  override def visitCompilationUnit(ctx: CompilationUnitContext): List[TopLevelStatement] = {
    val visitor = new TopLevelStatementVisitor
    ctx.topLevelStatement().asScala.map { topLevelStmt => visitor.visit(topLevelStmt) }
  }
}

class TopLevelStatementVisitor extends GrokBaseVisitor[TopLevelStatement] {
  val typeParameterVisit = (new TypeParameterVisitor).visit _
  val funcParameterVisit = (new FuncParameterVisitor).visit _
  val typeVisit = (new TypeVisitor).visit _
  val expressionVisit = (new ExpressionVisitor).visit _
  val fieldVisit = (new FieldVisitor).visit _
  val methodStubVisit = (new MethodStubVisitor).visit _
  val instanceMethodVisit = (new InstanceMethodVisitor).visit _
  val statementVisit = new (StatementVisitor).visit _

  override def visitTopLevelStatement(ctx: TopLevelStatementContext): TopLevelStatement = {
    val functionDefinition = nullToOption(ctx.functionDefinition()).map { visit(_) }
    val methodDefinition = nullToOption(ctx.methodDefintion()).map { visit(_) }
    val structDefinition = nullToOption(ctx.structDefinition()).map { visit(_) }
    val unionDefintion = nullToOption(ctx.unionDefintion()).map { visit(_) }
    val interfaceDefinition = nullToOption(ctx.interfaceDefinition()).map { visit(_) }
    val instance = nullToOption(ctx.instance()).map { visit(_) }
    val statement = nullToOption(ctx.statement()).map { visit(_) }
    functionDefinition
      .orElse(methodDefinition)
      .orElse(structDefinition)
      .orElse(unionDefintion)
      .orElse(interfaceDefinition)
      .orElse(instance)
      .orElse(statement)
      .get
  }

  override def visitFunctionDefinition(ctx: FunctionDefinitionContext): TopLevelStatement = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParameterVisit(_) }.toList.flatten
    val identifier = ctx.Identifier().getText
    val funcParameters = funcParameterVisit(ctx.funcParameters())
    val returnType = typeVisit(ctx.`type`())
    val body = expressionVisit(ctx.expression())
    FunctionDefinition(identifier, typeParameters, funcParameters, returnType, body)
  }

  override def visitMethodDefintion(ctx: MethodDefintionContext): TopLevelStatement = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParameterVisit(_) }.toList.flatten
    val receiver = typeVisit(ctx.`type`(0))
    val identifier = ctx.Identifier().getText
    val funcParameters = funcParameterVisit(ctx.funcParameters())
    val returnType = typeVisit(ctx.`type`(1))
    val body = expressionVisit(ctx.expression())
    MethodDefinition(receiver, identifier, typeParameters, funcParameters, returnType, body)
  }

  override def visitStructDefinition(ctx: StructDefinitionContext): TopLevelStatement = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParameterVisit(_) }.toList.flatten
    val identifier = ctx.Identifier().getText
    val fields = ctx.field().asScala.map { fieldVisit(_) }.toList
    StructDefinition(identifier, typeParameters, fields)
  }

  override def visitUnionDefintion(ctx: UnionDefintionContext): TopLevelStatement = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParameterVisit(_) }.toList.flatten
    val identifier = ctx.Identifier().getText
    val members = ctx.`type`().asScala.map { typeVisit(_) }.toList
    UnionDefinition(identifier, typeParameters, members)
  }

  override def visitInterfaceDefinition(ctx: InterfaceDefinitionContext): TopLevelStatement = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParameterVisit(_) }.toList.flatten
    val identifier = ctx.Identifier().getText
    val parent = typeVisit(ctx.`type`())
    val methods = ctx.methodStub().asScala.map { methodStubVisit(_) }.toList
    InterfaceDefinition(identifier, typeParameters, parent, methods)
  }

  override def visitInstance(ctx: InstanceContext): TopLevelStatement = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParameterVisit(_) }.toList.flatten
    val implementor = typeVisit(ctx.`type`(0))
    val interface = typeVisit(ctx.`type`(1))
    val methods = ctx.instanceMethod().asScala.map { instanceMethodVisit(_) }.toList
    Instance(implementor, interface, typeParameters, methods)
  }
  override def visitStatement(ctx: StatementContext): TopLevelStatement = statementVisit(ctx)
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
  override def visitExpression(ctx: ExpressionContext): Expression = ???
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

class TypeParameterVisitor extends GrokBaseVisitor[List[Type]] {
  override def visitTypeParameters(ctx: TypeParametersContext): List[Type] = ???
}

class FuncParameterVisitor extends GrokBaseVisitor[List[Parameter]] {
  override def visitFuncParameters(ctx: FuncParametersContext): List[Parameter] = ???
}

class TypeVisitor extends GrokBaseVisitor[Type] {
  override def visitType(ctx: TypeContext): Type = ???
}

class FieldVisitor extends GrokBaseVisitor[Field] {
  override def visitField(ctx: FieldContext): Field = ???
}

class MethodStubVisitor extends GrokBaseVisitor[MethodStub] {
  override def visitMethodStub(ctx: MethodStubContext): MethodStub = ???
}

class InstanceMethodVisitor extends GrokBaseVisitor[MethodDefinition] {
  override def visitInstanceMethod(ctx: InstanceMethodContext): MethodDefinition = ???
}

object Utilities {
  def nullToOption[T](nullable: T): Option[T] = nullable match {
    case null => None
    case notNull => Some(notNull)
  }

  def toString(identifier: TerminalNode): String = identifier.getText()
}