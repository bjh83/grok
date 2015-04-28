package com.grok

import scala.util.{Success, Try}

/**
 * Created by brendan.
 */
class TypeChecker extends ASTVisitor[Type, FinalDefinitionTable] {
  var definitionTable: FinalDefinitionTable = _
  var typeTable: TypeTable = _

  def visitAST(ast: List[TopLevelStatement], definitionTable: FinalDefinitionTable, typeTable: TypeTable): Unit = {
    this.definitionTable = definitionTable
    this.typeTable = typeTable
    ast.foreach(visitTopLevelStatement)
  }

  protected def visitFunctionDefinition(functionDefinition: FunctionDefinition): Type = {
    functionDefinition.parameters.foreach(param => definitionTable.addSymbol(ParameterSymbolDefinition(param)))
    internalVisitExpression(functionDefinition.definition)
    UnitType
  }

  protected def visitMethodDefinition(methodDefinition: MethodDefinition): Type = {
    sys.error("Methods are not yet implemented.")
  }

  protected def visitStructDefinition(structDefinition: StructDefinition): Type = UnitType

  protected def visitUnionDefinition(unionDefinition: UnionDefinition): Type = UnitType

  protected def visitInterfaceDefinition(interfaceDefinition: InterfaceDefinition): Type = UnitType

  protected def visitInstance(instance: Instance): Type = UnitType

  protected def visitVariableDeclaration(variableDeclaration: VariableDeclaration): Type = {
    val actualType = internalVisitExpression(variableDeclaration.value)
    variableDeclaration.varType match {
      case Some(expectedType) =>
        typeTable.derivesOrFail(actualType, expectedType)
        definitionTable.addSymbol(NormalVariableSymbolDefinition(variableDeclaration))
      case None =>
        val VariableDeclaration(mutability, name, _, value) = variableDeclaration
        val newVariableDeclaration = VariableDeclaration(mutability, name, Some(actualType), value)
        variableDeclaration.varType = Some(actualType)
        definitionTable.addSymbol(NormalVariableSymbolDefinition(newVariableDeclaration))
    }
    UnitType
  }

  protected def visitVariableAssignment(variableAssignment: VariableAssignment): Type = {
    val variableDefinition = definitionTable.lookupSymbol(VariableKey(variableAssignment.identifier))
    val actualType = internalVisitExpression(variableAssignment.value)
    typeTable.derivesOrFail(actualType, variableDefinition.`type`)
    variableAssignment.varType = variableDefinition.`type`
    UnitType
  }

  protected def visitStructAssignment(structAssignment: StructAssignment): Type = {
    val variableDefinition = definitionTable.lookupSymbol(VariableKey(structAssignment.identifier))
    val structDefinition = definitionTable.lookupStructDefinition(variableDefinition.`type`)
    val actualType = internalVisitExpression(structAssignment.value)
    structDefinition.fields.find(_.identifier == structAssignment.member) match {
      case Some(field) => typeTable.derives(actualType, field.fieldType)
      case None => sys.error("Field, " + structAssignment.member + ", does not exist.")
    }
    structAssignment.varType = variableDefinition.`type`
    structAssignment.structDef = structDefinition
    UnitType
  }

  protected def visitBlock(block: Block): Type = {
    block.statements.foreach(internalVisitStatement)
    block.expression.map(internalVisitExpression).getOrElse(UnitType)
  }

  override protected def internalVisitBlock(block: Block): Type = {
    assignType(super.internalVisitBlock)(block)
  }

  protected def visitLambda(lambda: Lambda): Type = {
    if (lambda.parameters.forall(_.paramType.isDefined)) {
      val definedParameters = lambda.parameters.map { case ParameterOptionalType(name, paramType) =>
        Parameter(name, paramType.get)
      }
      enterScope()
      definedParameters.foreach(param => definitionTable.addSymbol(ParameterSymbolDefinition(param)))
      val returnType = internalVisitExpression(lambda.expression)
      exitScope()
      FunctionType(definedParameters.map(_.paramType), returnType)
    } else {
      // If we cannot figure out the type, substitute the top and bottom types. We will have to figure out the actual
      // types later.
      FunctionType(lambda.parameters.map(_ => TopType), BottomType)
    }
  }

  private def assignLambdaType(lambda: Lambda, functionType: FunctionType): Unit = {
    lambda.`type` = functionType
  }

  private def assignLambdaType(lambdaTypeList: List[(Lambda, FunctionType)]): Unit = {
    lambdaTypeList.foreach { case (lambda, lambdaType) => assignLambdaType(lambda, lambdaType) }
  }

  protected def visitVariable(variable: Variable): Type = {
    val declaration: SymbolDefinition[_, Key] = definitionTable.lookupSymbolGroup(VariableKey(variable.identifier)) match {
      case functionGroup: FinalFunctionGroup => functionGroup.map.values.toList match {
        case List(functionDef) => functionDef
        case _ => sys.error("Function definition is ambiguous.")
      }
      case variableGroup: VariableGroup => NormalVariableSymbolDefinition(variableGroup.variableDeclaration)
      case _ => sys.error("Invalid symbol.")
    }
    declaration.`type`
  }

  override protected def internalVisitVariable(variable: Variable): Type = {
    assignType(super.internalVisitVariable)(variable)
  }

  protected def visitIfExpression(ifExpression: IfExpression): Type = {
    val (conditions, bodies, isUnit) = collapseIfExpression(ifExpression)
    conditions.foreach(internalVisitBooleanExpression)
    val bodyTypes = bodies.map(internalVisitExpression)
    if (isUnit) {
      UnitType
    } else {
      typeTable.computeUpperBound(bodyTypes.toSet)
    }
  }

  override protected def internalVisitIfExpression(ifExpression: IfExpression): Type = {
    assignType(super.internalVisitIfExpression)(ifExpression)
  }

  protected def visitWhileExpression(whileExpression: WhileExpression): Type = {
    internalVisitBooleanExpression(whileExpression.condition)
    internalVisitExpression(whileExpression.body)
  }

  override protected def internalVisitWhileExpression(whileExpression: WhileExpression): Type = {
    assignType(super.internalVisitWhileExpression)(whileExpression)
  }

  protected def visitMatchExpression(matchExpression: MatchExpression): Type = {
    val expressionType = internalVisitExpression(matchExpression.expression)
    val unionDefinition = definitionTable.lookupUnionDefinition(expressionType)
    val unionMembers = unionDefinition.members.toSet
    val caseMembers = matchExpression.cases.map(_.parameter.paramType).toSet
    if (unionMembers != caseMembers) {
      sys.error("Match does not handle all possible cases.")
    }
    val caseReturnTypes = matchExpression.cases.map(internalVisitCase)
    matchExpression.unionDef = unionDefinition
    typeTable.computeUpperBound(caseReturnTypes.toSet)
  }

  override protected def internalVisitMatchExpression(matchExpression: MatchExpression): Type = {
    assignType(super.internalVisitMatchExpression)(matchExpression)
  }

  protected def visitCase(caseExpression: Case): Type = {
    definitionTable.addSymbol(ParameterSymbolDefinition(caseExpression.parameter))
    internalVisitExpression(caseExpression.body)
  }

  override protected def internalVisitCase(caseExpression: Case): Type = {
    assignType(super.internalVisitCase)(caseExpression)
  }

  protected def visitFunctionCall(functionCall: FunctionCall): Type = {
    def localAssignLambdaType(params: List[Expression], functionSignature: FunctionType): Unit = {
      val lambdaTypePairing = params.zip(functionSignature.parameters).filter {
        case (lambda: Lambda, _) => true
        case _ => false
      }.asInstanceOf[List[(Lambda, FunctionType)]]
      assignLambdaType(lambdaTypePairing)
    }

    val paramTypes = functionCall.parameters.map(internalVisitExpression)
    val key = FunctionKey(functionCall.identifier, paramTypes)
    key.resolver = createResolver(functionCall.parameters)
    val definition = definitionTable.lookupSymbol(key)
    val signature = definition.`type`
    localAssignLambdaType(functionCall.parameters, signature.asInstanceOf[FunctionType])
    functionCall.functionDefinition = definition
    signature.asInstanceOf[FunctionType].returnType
  }

  override protected def internalVisitFunctionCall(functionCall: FunctionCall): Type = {
    assignType(super.internalVisitFunctionCall)(functionCall)
  }

  private def createResolver(parameters: List[Expression]): Set[FunctionType] => Set[FunctionType] = {
    (candidates: Set[FunctionType]) => {
      candidates.filter { functionType =>
        val typesToExpression = functionType.parameters.zip(parameters)
        val lambdas = typesToExpression.filter {
          case (_, expr: Lambda) => true
          case _ => false
        }.toSet.asInstanceOf[Set[(FunctionType, Lambda)]]
        lambdas.forall { case (signature, lambda) => canResolveLambdaWithSignature(lambda, signature) }
      }
    }
  }

  private def canResolveLambdaWithSignature(lambda: Lambda, signature: FunctionType): Boolean = {
    val Lambda(params, body) = lambda
    val newParams = params.zip(signature.parameters).map { case (ParameterOptionalType(name, _), newType) =>
      // TODO(Brendan): Should check whether original type exists and then whether the newType derives it.
      ParameterOptionalType(name, Some(newType))
    }
    Try(visitLambda(Lambda(newParams, body))) match {
      case Success(_) => true
      case _ => false
    }
  }

  protected def visitMethodCall(methodCall: MethodCall): Type = {
    sys.error("Methods are not yet implemented.")
  }

  override protected def internalVisitMethodCall(methodCall: MethodCall): Type = {
    assignType(super.internalVisitMethodCall)(methodCall)
  }

  protected def visitStructAccess(structAccess: StructAccess): Type = {
    val structType = internalVisitExpression(structAccess.receiver)
    val structDefinition = definitionTable.lookupStructDefinition(structType)
    val fieldType = structDefinition.fields.filter(_.identifier == structAccess.identifier) match {
      case List(Field(_, fieldType, _)) => fieldType
      case List() => sys.error("Could not find field named, " + structAccess.identifier)
      case _ => sys.error("Multiple fields resolved to name.")
    }
    structAccess.structDef = structDefinition
    fieldType
  }

  override protected def internalVisitStructAccess(structAccess: StructAccess): Type = {
    assignType(super.internalVisitStructAccess)(structAccess)
  }

  protected def visitThis(): Type = sys.error("Methods are not yet implemented.")

  protected def visitBooleanBinaryExpression(booleanBinaryExpression: BooleanBinaryExpression): Type = {
    val BooleanBinaryExpression(left, _, right) = booleanBinaryExpression
    if (internalVisitBooleanExpression(left) != BoolType) {
      sys.error(left + ", does not resolve to boolean expression.")
    }
    if (internalVisitBooleanExpression(right) != BoolType) {
      sys.error(right + ", does not resolve to boolean expression.")
    }
    BoolType
  }

  override protected def internalVisitBooleanBinaryExpression(booleanBinaryExpression: BooleanBinaryExpression): Type = {
    assignType(super.internalVisitBooleanBinaryExpression)(booleanBinaryExpression)
  }

  protected def visitBooleanInverse(booleanInverse: BooleanInverse): Type = {
    if (internalVisitBooleanExpression(booleanInverse.value) != BoolType) {
      sys.error(booleanInverse.value + ", does not resolve to boolean expression.")
    }
    BoolType
  }

  override protected def internalVisitBooleanInverse(booleanInverse: BooleanInverse): Type = {
    assignType(super.internalVisitBooleanInverse)(booleanInverse)
  }

  protected def visitBooleanComparison(booleanComparison: BooleanComparison): Type = {
    val BooleanComparison(left, _, right) = booleanComparison
    typeTable.derivesOrFail(internalVisitArithmeticExpression(left), FloatingPointType)
    if (!typeTable.derives(internalVisitArithmeticExpression(left), FloatingPointType)) {
      sys.error(left + ", does not resolve to arithmetic expression.")
    }
    if (!typeTable.derives(internalVisitArithmeticExpression(right), FloatingPointType)) {
      sys.error(right + ", does not resolve to arithmetic expression.")
    }
    BoolType
  }

  override def internalVisitBooleanComparison(booleanComparison: BooleanComparison): Type = {
    assignType(super.internalVisitBooleanComparison)(booleanComparison)
  }

  protected def visitBooleanExpressionWrapper(booleanExpressionWrapper: BooleanExpressionWrapper): Type = {
    val actualType = internalVisitExpression(booleanExpressionWrapper.expression)
    if (actualType != BoolType) {
      sys.error(actualType + ", does not resolve to arithmetic expression.")
    }
    BoolType
  }

  override protected def internalVisitBooleanExpressionWrapper(booleanExpressionWrapper: BooleanExpressionWrapper): Type = {
    assignType(super.internalVisitBooleanExpressionWrapper)(booleanExpressionWrapper)
  }

  protected def visitBooleanConstant(booleanConstant: BooleanConstant): Type = BoolType

  override protected def internalVisitBooleanConstant(booleanConstant: BooleanConstant): Type = {
    assignType(super.internalVisitBooleanConstant)(booleanConstant)
  }

  protected def visitArithmeticBinaryExpression(arithmeticBinaryExpression: ArithmeticBinaryExpression): Type = {
    val ArithmeticBinaryExpression(left, _, right) = arithmeticBinaryExpression
    (internalVisitArithmeticExpression(left), internalVisitArithmeticExpression(right)) match {
      case (FloatingPointType, FloatingPointType) => FloatingPointType
      case (FloatingPointType, IntegralType) => FloatingPointType
      case (IntegralType, FloatingPointType) => FloatingPointType
      case (IntegralType, IntegralType) => IntegralType
      case _ => sys.error("Type is not arithmetic type.")
    }
  }

  override protected def internalVisitArithmeticBinaryExpression(arithmeticBinaryExpression: ArithmeticBinaryExpression): Type = {
    assignType(super.internalVisitArithmeticBinaryExpression)(arithmeticBinaryExpression)
  }

  protected def visitArithmeticExpressionWrapper(arithmeticExpressionWrapper: ArithmeticExpressionWrapper): Type = {
    val actualType = internalVisitExpression(arithmeticExpressionWrapper.expression)
    typeTable.derivesOrFail(actualType, FloatingPointType)
    actualType
  }

  override protected def internalVisitArithmeticExpressionWrapper(arithmeticExpressionWrapper: ArithmeticExpressionWrapper): Type = {
    assignType(super.internalVisitArithmeticExpressionWrapper)(arithmeticExpressionWrapper)
  }

  protected def visitArithmeticIntegralConstant(arithmeticIntegralConstant: ArithmeticIntegralConstant): Type = IntegralType

  override protected def internalVisitArithmeticIntegralConstant(arithmeticIntegralConstant: ArithmeticIntegralConstant): Type = {
    assignType(super.internalVisitArithmeticIntegralConstant)(arithmeticIntegralConstant)
  }

  protected def visitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant): Type = FloatingPointType

  override protected def internalVisitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant): Type = {
    assignType(super.internalVisitArithmeticFloatingPointConstant)(arithmeticFloatingPointConstant)
  }

  private def assignType[T <: Expression](visitor: T => Type)(expr: T): Type = {
    val result = visitor(expr)
    expr.`type` = result
    result
  }
}