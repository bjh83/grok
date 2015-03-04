package com.grok

import com.grok.GrokParser._
import com.grok.Utilities._

import scala.collection.JavaConverters._

class ASTBuilder extends GrokBaseVisitor[List[TopLevelStatement]] {
  override def visitCompilationUnit(ctx: CompilationUnitContext): List[TopLevelStatement] = {
    val visitor = new TopLevelStatementVisitor
    ctx.topLevelStatement().asScala.map { topLevelStmt => visitor.visit(topLevelStmt) }.toList
  }
}

class TopLevelStatementVisitor extends GrokBaseVisitor[TopLevelStatement] {
  val typeParameterVisit = (new TypeParametersVisitor).visit _
  val funcParameterVisit = (new FuncParametersVisitor).visit _
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
    val methods = ctx.instanceMethod().asScala.map { method => instanceMethodVisit(method)(implementor) }.toList
    Instance(implementor, interface, typeParameters, methods)
  }

  override def visitStatement(ctx: StatementContext): TopLevelStatement = statementVisit(ctx)
}

class StatementVisitor extends GrokBaseVisitor[Statement] {
  val expressionVisit = (new ExpressionVisitor).visit _
  val typeVisit = (new TypeVisitor).visit _

  override def visitStatement(ctx: StatementContext): Statement = visit(ctx.innerStatement())

  override def visitInnerStatement(ctx: InnerStatementContext): Statement = {
    nullToOption(ctx.variableDeclaration()).map { visit }
      .orElse(nullToOption(ctx.variableAssignment()).map { visit })
      .orElse(nullToOption(ctx.structAssignment()).map { visit })
      .orElse(nullToOption(ctx.ifExpression()).map { visit })
      .orElse(nullToOption(ctx.whileExpression()).map { visit })
      .orElse(nullToOption(ctx.matchExpression()).map { visit })
      .orElse(nullToOption(ctx.functionCall()).map { visit })
      .get
  }

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Statement = {
    val mutability = parseMutability(ctx.modifier.getText)
    val identifier = ctx.Identifier().getText
    val varType = nullToOption(ctx.`type`()).map { typeVisit(_) }
    val value = expressionVisit(ctx.expression())
    VariableDeclaration(mutability, identifier, varType, value)
  }

  override def visitVariableAssignment(ctx: VariableAssignmentContext): Statement = {
    val identifier = ctx.Identifier().getText
    val value = expressionVisit(ctx.expression())
    VariableAssignment(identifier, value)
  }
  override def visitStructAssignment(ctx: StructAssignmentContext): Statement = {
    val identifier = ctx.Identifier(0).getText
    val member = ctx.Identifier(1).getText
    val value = expressionVisit(ctx.expression())
    StructAssignment(identifier, member, value)
  }

  override def visitIfExpression(ctx: IfExpressionContext): Statement = ExpressionWrapper(expressionVisit(ctx))
  override def visitWhileExpression(ctx: WhileExpressionContext): Statement = ExpressionWrapper(expressionVisit(ctx))
  override def visitMatchExpression(ctx: MatchExpressionContext): Statement = ExpressionWrapper(expressionVisit(ctx))
  override def visitFunctionCall(ctx: FunctionCallContext): Statement = ExpressionWrapper(expressionVisit(ctx))
}

class ExpressionVisitor extends GrokBaseVisitor[Expression] {
  val arithmeticExpressionVisit = (new ArithmeticExpressionVisitor).visit _
  val booleanExpressionVisit = (new BooleanExpressionVisitor).visit _
  val blockVisit = (new BlockVisitor).visit _
  val caseVisit = (new CaseVisitor).visit _
  val lambdaParameterVisit = (new LambdaParametersVisitor).visit _

  override def visitExpression(ctx: ExpressionContext): Expression = {
    nullToOption(ctx.ifExpression()).map { visit }
      .orElse(nullToOption(ctx.whileExpression()).map { visit })
      .orElse(nullToOption(ctx.matchExpression()).map { visit })
      .orElse(nullToOption(ctx.functionCall()).map { visit })
      .orElse(nullToOption(ctx.block()).map { visit })
      .orElse(nullToOption(ctx.lambda()).map { visit })
      .orElse(nullToOption(ctx.booleanExpression()).map { visit })
      .orElse(nullToOption(ctx.arithmeticExpression()).map { visit })
      .orElse(nullToOption(ctx.variable()).map { visit })
      .orElse(nullToOption(ctx.thisExpression()).map { visit })
      .get
  }

  override def visitIfExpression(ctx: IfExpressionContext): Expression = {
    val condition = booleanExpressionVisit(ctx.booleanExpression())
    val block = blockVisit(ctx.block(0))
    val alternative = nullToOption(ctx.ifExpression()).map { visit }
      .orElse(nullToOption(ctx.block(1)).map { blockVisit })
    IfExpression(condition, block, alternative)
  }

  override def visitWhileExpression(ctx: WhileExpressionContext): Expression = {
    val condition = booleanExpressionVisit(ctx.booleanExpression())
    val body = blockVisit(ctx.block())
    WhileExpression(condition, body)
  }

  override def visitMatchExpression(ctx: MatchExpressionContext): Expression = {
    val expression = visit(ctx.expression())
    val cases = ctx.matchCase().asScala.map { caseVisit }.toList
    MatchExpression(expression, cases)
  }

  override def visitFunctionCall(ctx: FunctionCallContext): Expression = {
    val identifier = ctx.Identifier().getText
    val arguments = ctx.arguments().expression().asScala.map { visit }.toList
    FunctionCall(identifier, arguments)
  }

  override def visitLambda(ctx: LambdaContext): Expression = {
    val parameters = lambdaParameterVisit(ctx.lambdaParameters())
    val body = visit(ctx.expression())
    Lambda(parameters, body)
  }

  override def visitBooleanExpression(ctx: BooleanExpressionContext): Expression = booleanExpressionVisit(ctx)

  override def visitArithmeticExpression(ctx: ArithmeticExpressionContext): Expression = arithmeticExpressionVisit(ctx)

  override def visitVariable(ctx: VariableContext): Expression = Variable(ctx.Identifier().getText)

  override def visitThisExpression(ctx: ThisExpressionContext): Expression = This
}

class BooleanExpressionVisitor extends GrokBaseVisitor[BooleanExpression] {
  val expressionVisit = (new ExpressionVisitor).visit _
  val arithmeticExpressionVisit = (new ArithmeticExpressionVisitor).visit _

  override def visitBooleanExpression(ctx: BooleanExpressionContext): BooleanExpression = {
    val left = visit(ctx.booleanProduct())
    val right = nullToOption(ctx.booleanExpression()).map { visit }
    if (right.nonEmpty) {
      BooleanBinaryExpression(left, OR, right.get)
    } else {
      left
    }
  }

  override def visitBooleanProduct(ctx: BooleanProductContext): BooleanExpression = {
    val left = visit(ctx.booleanInverse())
    val right = nullToOption(ctx.booleanProduct()).map { visit }
    if (right.nonEmpty) {
      BooleanBinaryExpression(left, AND, right.get)
    } else {
      left
    }
  }

  override def visitBooleanInverse(ctx: BooleanInverseContext): BooleanExpression = {
    val value = visit(ctx.booleanTerm())
    if (ctx.inverse != null) {
      BooleanInverse(value)
    } else {
      value
    }
  }

  override def visitBooleanTerm(ctx: BooleanTermContext): BooleanExpression = {
    nullToOption(ctx.booleanExpression()).map { visit }
      .orElse(nullToOption(ctx.comparison()).map { visit })
      .orElse(nullToOption(ctx.functionCall()).map { call => BooleanFunctionCallWrapper(expressionVisit(call)) })
      .orElse(nullToOption(ctx.variable()).map { variable => BooleanVariableWrapper(expressionVisit(variable))})
      .orElse(nullToOption(ctx.BooleanConstant()).map { const => com.grok.BooleanConstant(const.getText.toBoolean) })
      .get
  }

  override def visitComparison(ctx: ComparisonContext): BooleanExpression = {
    val left = arithmeticExpressionVisit(ctx.arithmeticExpression(0))
    val operator = parseComparisonOperator(ctx.operation.getText)
    val right = arithmeticExpressionVisit(ctx.arithmeticExpression(1))
    BooleanComparison(left, operator, right)
  }
}

class ArithmeticExpressionVisitor extends GrokBaseVisitor[ArithmeticExpression] {
  val expressionVisit = (new ExpressionVisitor).visit _

  override def visitArithmeticExpression(ctx: ArithmeticExpressionContext): ArithmeticExpression = {
    val left = visit(ctx.arithmeticProduct())
    val operator = nullToOption(ctx.operation).map { operator => parseArithmeticOperator(operator.getText) }
    val right = nullToOption(ctx.arithmeticExpression()).map { visit }
    if (right.nonEmpty) {
      ArithmeticBinaryExpression(left, operator.get, right.get)
    } else {
      left
    }
  }

  override def visitArithmeticProduct(ctx: ArithmeticProductContext): ArithmeticExpression = {
    val left = visit(ctx.arithmeticTerm())
    val operator = nullToOption(ctx.operation).map { operator => parseArithmeticOperator(operator.getText) }
    val right = nullToOption(ctx.arithmeticProduct()).map { visit }
    if (right.nonEmpty) {
      ArithmeticBinaryExpression(left, operator.get, right.get)
    } else {
      left
    }
  }

  override def visitArithmeticTerm(ctx: ArithmeticTermContext): ArithmeticExpression = {
    nullToOption(ctx.arithmeticExpression()).map { visit }
      .orElse(nullToOption(ctx.functionCall()).map { call => ArithmeticFunctionCallWrapper(expressionVisit(call)) })
      .orElse(nullToOption(ctx.variable()).map { variable => ArithmeticVariableWrapper(expressionVisit(variable)) })
      .orElse(nullToOption(ctx.ArithmeticConstant()).map { const => com.grok.ArithmeticConstant(const.getText.toDouble) })
      .get
  }
}

class BlockVisitor extends GrokBaseVisitor[Block] {
  val statementVisit = (new StatementVisitor).visit _
  val expressionVisit = (new ExpressionVisitor).visit _

  override def visitBlock(ctx: BlockContext): Block = {
    val statements = ctx.statement().asScala.map { statementVisit }.toList
    val expression = nullToOption(ctx.expression()).map { expressionVisit }
    Block(statements, expression)
  }
}

class CaseVisitor extends GrokBaseVisitor[Case] {
  val typeVisit = (new TypeVisitor).visit _
  val expressionVisit = (new ExpressionVisitor).visit _
  override def visitMatchCase(ctx: MatchCaseContext): Case = {
    val parameter = ctx.Identifier().getText
    val paramType = typeVisit(ctx.`type`())
    val body = expressionVisit(ctx.expression())
    Case(Parameter(parameter, paramType), body)
  }
}

class TypeParametersVisitor extends GrokBaseVisitor[List[Type]] {
  val typeVisit = (new TypeVisitor).visit _

  override def visitTypeParameters(ctx: TypeParametersContext): List[Type] = ctx.`type`().asScala.map { typeVisit }.toList
}

class FuncParametersVisitor extends GrokBaseVisitor[List[Parameter]] {
  val funcParameterVisit = (new FuncParameterVisitor).visit _

  override def visitFuncParameters(ctx: FuncParametersContext): List[Parameter] = {
    ctx.funcParameter().asScala.map { funcParameterVisit }.toList
  }
}

class FuncParameterVisitor extends GrokBaseVisitor[Parameter] {
  val typeVisit = (new TypeVisitor).visit _
  override def visitFuncParameter(ctx: FuncParameterContext): Parameter = {
    val identifier = ctx.Identifier().getText
    val paramType = typeVisit(ctx.`type`())
    Parameter(identifier, paramType)
  }
}

class LambdaParametersVisitor extends GrokBaseVisitor[List[ParameterOptionalType]] {
  val lambdaParameterVisit = (new LambdaParameterVisitor).visit _

  override def visitLambdaParameters(ctx: LambdaParametersContext): List[ParameterOptionalType] = {
    ctx.lambdaParameter().asScala.map { lambdaParameterVisit }.toList
  }
}

class LambdaParameterVisitor extends GrokBaseVisitor[ParameterOptionalType] {
  val typeVisit = (new TypeVisitor).visit _
  override def visitLambdaParameter(ctx: LambdaParameterContext): ParameterOptionalType = {
    val identifier = ctx.Identifier().getText
    val paramType = nullToOption(ctx.`type`()).map { typeVisit }
    ParameterOptionalType(identifier, paramType)
  }
}

class TypeVisitor extends GrokBaseVisitor[Type] {
  val typeParametersVisit = (new TypeParametersVisitor).visit _

  override def visitType(ctx: TypeContext): Type = {
    val identifier = ctx.Identifier().getText
    val typeParams = typeParametersVisit(ctx.typeParameters())
    Type(identifier, typeParams)
  }
}

class FieldVisitor extends GrokBaseVisitor[Field] {
  val typeVisit = (new TypeVisitor).visit _
  override def visitField(ctx: FieldContext): Field = {
    val mutability = parseMutability(ctx.modifier.getText)
    val identifier = ctx.Identifier().getText
    val fieldType = typeVisit(ctx.`type`())
    Field(identifier, fieldType, mutability)
  }
}

class MethodStubVisitor extends GrokBaseVisitor[MethodStub] {
  val typeParametersVisit = (new TypeParametersVisitor).visit _
  val funcParametersVisit = (new FuncParametersVisitor).visit _
  val typeVisit = (new TypeVisitor).visit _

  override def visitMethodStub(ctx: MethodStubContext): MethodStub = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParametersVisit }.toList.flatten
    val identifier = ctx.Identifier().getText
    val funcParameters = funcParametersVisit(ctx.funcParameters())
    val returnType = typeVisit(ctx.`type`())
    MethodStub(identifier, typeParameters, funcParameters, returnType)
  }
}

class InstanceMethodVisitor extends GrokBaseVisitor[Type => MethodDefinition] {
  val typeParametersVisit = (new TypeParametersVisitor).visit _
  val funcParametersVisit = (new FuncParametersVisitor).visit _
  val typeVisit = (new TypeVisitor).visit _
  val expressionVist = (new ExpressionVisitor).visit _

  override def visitInstanceMethod(ctx: InstanceMethodContext): Type => MethodDefinition = {
    val typeParameters = nullToOption(ctx.typeParameters()).map { typeParametersVisit }.toList.flatten
    val identifier = ctx.Identifier().getText
    val funcParameters = funcParametersVisit(ctx.funcParameters())
    val returnType = typeVisit(ctx.`type`())
    val body = expressionVist(ctx.expression())
    receiver => MethodDefinition(receiver, identifier, typeParameters, funcParameters, returnType, body)
  }
}

object Utilities {
  def nullToOption[T](nullable: T): Option[T] = nullable match {
    case null => None
    case notNull => Some(notNull)
  }

  val parseMutability: PartialFunction[String, Mutability] = {
    case "val" => IMMUTABLE
    case "var" => MUTABLE
  }

  val parseComparisonOperator: PartialFunction[String, ComparisonOperator] = {
    case "==" => EQUALS
    case "!=" => NOT_EQUALS
    case "<" => LESS
    case ">" => GREATER
    case "<=" => LESS_OR_EQUAL
    case ">=" => GREATER_OR_EQUAL
  }

  val parseArithmeticOperator: PartialFunction[String, ArithmeticOperator] = {
    case "+" => PLUS
    case "-" => MINUS
    case "*" => MULTIPLY
    case "/" => DIVIDE
    case "%" => MODULUS
  }
}