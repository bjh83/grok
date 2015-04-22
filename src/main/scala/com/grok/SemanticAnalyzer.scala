package com.grok

/**
 * Created by brendan.
 */
class SemanticAnalyzer extends ASTVisitor[Unit, InitialDefinitionTable] {
  var definitionTable: InitialDefinitionTable = _

  def visitAST(ast: List[TopLevelStatement]): InitialDefinitionTable = {
    definitionTable = new InitialDefinitionTable
    ast.foreach(visitTopLevelStatement)
    definitionTable
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
    val params = fields.map { case Field(fieldName, fieldType, _) => Parameter(fieldName, fieldType) }
    val returnType = TypeTableFactory.structToType(structDefinition)
    val paramsAsVariables = params.map { param =>
      val variable = Variable(param.identifier)
      variable.`type` = param.paramType
      variable
    }
    definitionTable.addSymbol(FunctionSymbolDefinition(FunctionDefinition(
      name,
      typeParams,
      params,
      returnType,
      StructConstructor(structDefinition, paramsAsVariables))))
  }

  protected def visitUnionDefinition(unionDefinition: UnionDefinition): Unit = {
    definitionTable.addDefinition(unionDefinition)
    val UnionDefinition(name, typeParams, members) = unionDefinition
    // TODO: Write actual munging function.
    val params = members.map(member => Parameter("$" + member, member))
    val returnType = TypeTableFactory.unionToType(unionDefinition)
    val paramsToVariables = params.map { param =>
      val variable = Variable(param.identifier)
      variable.`type` = param.paramType
      (param, variable)
    }
    paramsToVariables.foreach { case (param, variable) =>
      definitionTable.addSymbol(FunctionSymbolDefinition(FunctionDefinition(
        name,
        typeParams,
        List(param),
        returnType,
        UnionConstructor(unionDefinition, variable)
      )))
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

  protected def visitFunctionCall(functionCall: FunctionCall): Unit = {
    // TODO(Brendan): Even though this works in practice, we should provide a simple key constructor for function calls
    // when type information does not yet exist.
    definitionTable.containsSymbolFail(VariableKey(functionCall.identifier))
  }

  protected def visitMethodCall(methodCall: MethodCall): Unit = {
    // TODO(Brendan): Even though this works in practice, we should provide a simple key constructor for function calls
    // when type information does not yet exist.
    definitionTable.containsSymbolFail(VariableKey(methodCall.identifier))
  }

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