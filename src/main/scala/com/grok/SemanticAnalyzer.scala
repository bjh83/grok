package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
class SemanticAnalyzer extends ASTVisitor[Unit, InitialDefinitionTable] {
  var definitionTable: InitialDefinitionTable = _
  var builtInFunctions = mutable.MutableList[FunctionDefinition]()

  def visitAST(ast: List[TopLevelStatement]): (InitialDefinitionTable, List[FunctionDefinition]) = {
    definitionTable = new InitialDefinitionTable
    addPrintFunctions()
    ast.foreach(visitTopLevelStatement)
    (definitionTable, builtInFunctions.toList)
  }

  private def addPrintFunctions(): Unit = {
    val intVariable = Variable("value")
    intVariable.`type` = IntegralType
    val printInt = FunctionDefinition(
      "println",
      List(),
      List(Parameter("value", IntegralType)),
      UnitType,
      PrintExpression(intVariable)
    )
    definitionTable.addSymbol(FunctionSymbolDefinition(printInt))
    builtInFunctions += printInt

    val floatVariable = Variable("value")
    floatVariable.`type` = FloatingPointType
    val printFloat = FunctionDefinition(
      "println",
      List(),
      List(Parameter("value", FloatingPointType)),
      UnitType,
      PrintExpression(floatVariable)
    )
    definitionTable.addSymbol(FunctionSymbolDefinition(printFloat))
    builtInFunctions += printFloat

    val boolVariable = Variable("value")
    boolVariable.`type` = BoolType
    val printBool = FunctionDefinition(
      "println",
      List(),
      List(Parameter("value", BoolType)),
      UnitType,
      PrintExpression(boolVariable)
    )
    definitionTable.addSymbol(FunctionSymbolDefinition(printBool))
    builtInFunctions += printBool
  }

  override protected def internalVisitFunctionDefinition(functionDefinition: FunctionDefinition): Unit = {
    definitionTable.addSymbol(FunctionSymbolDefinition(functionDefinition))
    super.internalVisitFunctionDefinition(functionDefinition)
  }

  override protected def visitFunctionDefinition(functionDefinition: FunctionDefinition): Unit = {
    functionDefinition.parameters.foreach(param => definitionTable.addSymbol(ParameterSymbolDefinition(param)))
    internalVisitExpression(functionDefinition.definition)
  }

  protected def visitMethodDefinition(methodDefinition: MethodDefinition): Unit = {
    sys.error("Methods are not yet implemented.")
  }

  protected def visitStructDefinition(structDefinition: StructDefinition): Unit = {
    definitionTable.addDefinition(structDefinition)
    val StructDefinition(name, typeParams, fields) = structDefinition
    if (fields.exists(_.fieldType == UnitType)) {
      sys.error("Field may not take on type Unit")
    }
    val params = fields.map { case Field(fieldName, fieldType, _) => Parameter(fieldName, fieldType) }
    val returnType = TypeTableFactory.structToType(structDefinition)
    val paramsAsVariables = params.map { param =>
      val variable = Variable(param.identifier)
      variable.`type` = param.paramType
      variable
    }
    val constructor = FunctionDefinition(
      name,
      typeParams,
      params,
      returnType,
      StructConstructor(structDefinition, paramsAsVariables)
    )
    definitionTable.addSymbol(FunctionSymbolDefinition(constructor))
    builtInFunctions += constructor
  }

  protected def visitUnionDefinition(unionDefinition: UnionDefinition): Unit = {
    definitionTable.addDefinition(unionDefinition)
    val UnionDefinition(name, typeParams, members) = unionDefinition
    if (members.contains(UnitType)) {
      sys.error("Union may not contain Unit")
    }
    // TODO: Write actual munging function.
    val params = members.map(member => Parameter("$" + member, member))
    val returnType = TypeTableFactory.unionToType(unionDefinition)
    val paramsToVariables = params.map { param =>
      val variable = Variable(param.identifier)
      variable.`type` = param.paramType
      (param, variable)
    }
    paramsToVariables.foreach { case (param, variable) =>
      val constructor = FunctionDefinition(
        name,
        typeParams,
        List(param),
        returnType,
        UnionConstructor(unionDefinition, variable)
      )
      definitionTable.addSymbol(FunctionSymbolDefinition(constructor))
      builtInFunctions += constructor
    }
  }

  protected def visitInterfaceDefinition(interfaceDefinition: InterfaceDefinition): Unit = {
    definitionTable.addDefinition(interfaceDefinition)
  }

  protected def visitInstance(instance: Instance): Unit = {
    definitionTable.addDefinition(instance)
  }

  override protected def visitVariableDeclaration(variableDeclaration: VariableDeclaration): Unit = {
    internalVisitExpression(variableDeclaration.value)
    definitionTable.addSymbol(NormalVariableSymbolDefinition(variableDeclaration))
  }

  protected def visitVariableAssignment(variableAssignment: VariableAssignment): Unit = {
    definitionTable.containsSymbolFail(VariableKey(variableAssignment.identifier))
    internalVisitExpression(variableAssignment.value)
  }

  protected def visitStructAssignment(structAssignment: StructAssignment): Unit = {
    definitionTable.containsSymbolFail(VariableKey(structAssignment.identifier))
    internalVisitExpression(structAssignment.value)
  }

  protected def visitBlock(block: Block): Unit = {
    block.statements.map(internalVisitStatement)
    block.expression.map(internalVisitExpression)
  }

  protected def visitLambda(lambda: Lambda): Unit = ()

  protected def visitVariable(variable: Variable): Unit = {
    definitionTable.containsSymbolFail(VariableKey(variable.identifier))
  }

  protected def visitIfExpression(ifExpression: IfExpression): Unit = {
    internalVisitBooleanExpression(ifExpression.condition)
    internalVisitExpression(ifExpression.body)
    ifExpression.alternative.map(internalVisitExpression)
  }

  protected def visitWhileExpression(whileExpression: WhileExpression): Unit = {
    internalVisitBooleanExpression(whileExpression.condition)
    internalVisitExpression(whileExpression.body)
  }

  protected def visitMatchExpression(matchExpression: MatchExpression): Unit = {
    internalVisitExpression(matchExpression.expression)
    matchExpression.cases.map(internalVisitCase)
  }

  protected def visitCase(caseExpression: Case): Unit = ()

  // Cannot assert existence of a definition which may not yet have been scanned.
  protected def visitFunctionCall(functionCall: FunctionCall): Unit = ()

  // Cannot assert existence of a definition which may not yet have been scanned.
  protected def visitMethodCall(methodCall: MethodCall): Unit = ()

  protected def visitStructAccess(structAccess: StructAccess): Unit = {
    definitionTable.containsSymbolFail(VariableKey(structAccess.identifier))
  }

  protected def visitThis(): Unit = ()

  protected def visitBooleanBinaryExpression(booleanBinaryExpression: BooleanBinaryExpression): Unit = {
    internalVisitBooleanExpression(booleanBinaryExpression.left)
    internalVisitBooleanExpression(booleanBinaryExpression.right)
  }

  protected def visitBooleanInverse(booleanInverse: BooleanInverse): Unit = {
    internalVisitBooleanExpression(booleanInverse.value)
  }

  protected def visitBooleanComparison(booleanComparison: BooleanComparison): Unit = {
    internalVisitArithmeticExpression(booleanComparison.left)
    internalVisitArithmeticExpression(booleanComparison.right)
  }

  protected def visitBooleanExpressionWrapper(booleanExpressionWrapper: BooleanExpressionWrapper): Unit = {
    internalVisitExpression(booleanExpressionWrapper)
  }

  protected def visitBooleanConstant(booleanConstant: BooleanConstant): Unit = ()

  protected def visitArithmeticBinaryExpression(arithmeticBinaryExpression: ArithmeticBinaryExpression): Unit = {
    internalVisitArithmeticExpression(arithmeticBinaryExpression.left)
    internalVisitArithmeticExpression(arithmeticBinaryExpression.right)
  }

  protected def visitArithmeticExpressionWrapper(arithmeticExpressionWrapper: ArithmeticExpressionWrapper): Unit = {
    internalVisitExpression(arithmeticExpressionWrapper.expression)
  }

  protected def visitArithmeticIntegralConstant(arithmeticIntegralConstant: ArithmeticIntegralConstant): Unit = ()

  protected def visitArithmeticFloatingPointConstant(arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant): Unit = ()
}