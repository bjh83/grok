package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
class CodeGenerator {
  private val functionLabelMap = mutable.Map[FunctionDefinition, Label]()
  private var tempCount = 0

  private def newTempInt(): IntTempOperand = {
    val temp = IntTempOperand(tempCount)
    tempCount += 1
    temp
  }

  private def newTempFloat(): FloatTempOperand = {
    val temp = FloatTempOperand(tempCount)
    tempCount += 1
    temp
  }

  private def newTempBool(): BoolTempOperand = {
    val temp = BoolTempOperand(tempCount)
    tempCount += 1
    temp
  }

  private def newTempReference(): ReferenceTempOperand = {
    val temp = ReferenceTempOperand(tempCount)
    tempCount += 1
    temp
  }

  private def newTemp(`type`: Type): Operand = {
    `type` match {
      case IntegralType => newTempInt()
      case FloatingPointType => newTempFloat()
      case BoolType => newTempBool()
      case UnitType => UnitOperand
      case _ => newTempReference()
    }
  }

  private def newRealOperand(name: String, `type`: Type): Operand = {
    `type` match {
      case IntegralType => IntRealOperand(name)
      case FloatingPointType => FloatRealOperand(name)
      case BoolType => BoolRealOperand(name)
      case _ => ReferenceRealOperand(name)
    }
  }

  def visitAST(ast: List[TopLevelStatement]): (CodeBlock, Map[String, Label]) = {
    val block = ast
      .map(visitTopLevelStatement)
      .reduce((left, right) => left.append(right))
    val main = functionLabelMap.keys.find(_.identifier == "main").getOrElse(sys.error("Must provide main method!"))
    (CodeBlock(CallFunc(functionLabelMap(main))).append(block), functionLabelMap.map {
      case (definition, label) => (definition.identifier, label)
    }.toMap)
  }

  protected def visitArithmeticExpression[T <: ArithmeticOperand](arithmeticExpression: ArithmeticExpression, operand: T): CodeBlock = {
    arithmeticExpression match {
      case expr: ArithmeticBinaryExpression => visitArithmeticBinaryExpression(expr, operand)
      case expr: ArithmeticExpressionWrapper => visitArithmeticExpressionWrapper(expr, operand)
      case expr: ArithmeticIntegralConstant => visitArithmeticIntegralConstant(expr, operand)
      case expr: ArithmeticFloatingPointConstant => visitArithmeticFloatingPointConstant(expr, operand)
    }
  }

  protected def visitBooleanExpression(booleanExpression: BooleanExpression, operand: BoolOperand): CodeBlock = {
    booleanExpression match {
      case expr: BooleanBinaryExpression => visitBooleanBinaryExpression(expr, operand)
      case expr: BooleanInverse => visitBooleanInverse(expr, operand)
      case expr: BooleanComparison => visitBooleanComparison(expr, operand)
      case expr: BooleanExpressionWrapper => visitBooleanExpressionWrapper(expr, operand)
      case expr: BooleanConstant => visitBooleanConstant(expr, operand)
    }
  }

  protected def visitExpression(expression: Expression, operand: Operand): CodeBlock = {
    expression match {
      case expr: Block => visitBlock(expr, operand)
      case expr: Lambda => visitLambda(expr, operand)
      case expr: BooleanExpression => visitBooleanExpression(expr, operand.asInstanceOf[BoolOperand])
      case expr: ArithmeticExpression => visitArithmeticExpression(expr, operand.asInstanceOf[ArithmeticOperand])
      case expr: Variable => visitVariable(expr, operand)
      case expr: IfExpression => visitIfExpression(expr, operand)
      case expr: WhileExpression => visitWhileExpression(expr, operand)
      case expr: MatchExpression => visitMatchExpression(expr, operand)
      case expr: FunctionCall => visitFunctionCall(expr, operand)
      case expr: MethodCall => visitMethodCall(expr, operand)
      case expr: StructAccess => visitStructAccess(expr, operand)
      case This => visitThis(operand)
      case expr: Case => visitCase(expr, null, -1, operand)
      case expr: StructConstructor => visitStructConstructor(expr, operand.asInstanceOf[ReferenceOperand])
      case expr: UnionConstructor => visitUnionConstructor(expr, operand.asInstanceOf[ReferenceOperand])
      case expr: PrintExpression => visitPrintExpression(expr)
    }
  }

  protected def visitStatement(statement: Statement): CodeBlock = {
    statement match {
      case stat: VariableDeclaration => visitVariableDeclaration(stat)
      case stat: VariableAssignment => visitVariableAssignment(stat)
      case stat: StructAssignment => visitStructAssignment(stat)
      case stat: ExpressionWrapper => visitExpressionWrapper(stat)
    }
  }

  protected def visitTopLevelStatement(statement: TopLevelStatement): CodeBlock = {
    statement match {
      case functionDefinition: FunctionDefinition => visitFunctionDefinition(functionDefinition)
      case methodDefinition: MethodDefinition => visitMethodDefinition(methodDefinition)
      case structDefinition: StructDefinition => visitStructDefinition(structDefinition)
      case unionDefinition: UnionDefinition => visitUnionDefinition(unionDefinition)
      case interfaceDefinition: InterfaceDefinition => visitInterfaceDefinition(interfaceDefinition)
      case instance: Instance => visitInstance(instance)
      case statement: Statement => visitStatement(statement)
    }
  }

  protected def visitFunctionDefinition(functionDefinition: FunctionDefinition): CodeBlock = {
    val params = functionDefinition.parameters.map(param => newRealOperand(param.identifier, param.paramType))
    val paramBlock = params.reverse.zipWithIndex // Must take params off in reverse order from stack.
      .map {
        case (operand: IntOperand, index) => ParamFromStackInt(operand, index)
        case (operand: FloatOperand, index) => ParamFromStackFloat(operand, index)
        case (operand: BoolOperand, index) => ParamFromStackBool(operand, index)
        case (operand: ReferenceOperand, index) => ParamFromStackReference(operand, index)
      }
      .foldLeft(CodeBlock())((left, right) => left.append(right))

    // TODO: Account for unit return type.
    val returnOperand = newTemp(functionDefinition.returnType)
    paramBlock.append(visitExpression(functionDefinition.definition, returnOperand))
    returnOperand match {
      case operand: IntOperand => paramBlock.append(ReturnInt(operand))
      case operand: FloatOperand => paramBlock.append(ReturnFloat(operand))
      case operand: BoolOperand => paramBlock.append(ReturnBool(operand))
      case operand: ReferenceOperand => paramBlock.append(ReturnReference(operand))
      case UnitOperand => paramBlock.append(Return)
    }
    functionLabelMap(functionDefinition) = paramBlock.blockStart
    paramBlock
  }

  protected def visitMethodDefinition(methodDefinition: MethodDefinition): CodeBlock = {
    sys.error("Methods are not yet supported.")
  }

  // We do not need to do anything here.
  protected def visitStructDefinition(structDefinition: StructDefinition): CodeBlock = CodeBlock()

  protected def visitUnionDefinition(unionDefinition: UnionDefinition): CodeBlock = CodeBlock()

  protected def visitInterfaceDefinition(interfaceDefinition: InterfaceDefinition): CodeBlock = {
    sys.error("Interface not yet supported.")
  }

  protected def visitInstance(instance: Instance): CodeBlock = {
    sys.error("Instance not yet supported.")
  }

  protected def visitVariableDeclaration(variableDeclaration: VariableDeclaration): CodeBlock = {
    val operand = newRealOperand(variableDeclaration.identifier, variableDeclaration.varType.get)
    CodeBlock(visitExpression(variableDeclaration.value, operand))
  }

  protected def visitVariableAssignment(variableAssignment: VariableAssignment): CodeBlock = {
    val operand = newRealOperand(variableAssignment.identifier, variableAssignment.varType)
    CodeBlock(visitExpression(variableAssignment.value, operand))
  }

  protected def visitStructAssignment(structAssignment: StructAssignment): CodeBlock = {
    val structOperand = ReferenceRealOperand(structAssignment.identifier)
    val memberOffset = structAssignment.structDef.fields.map(_.identifier).indexOf(structAssignment.member)
    val tempOperand = newTemp(structAssignment.value.`type`)
    val valueBlock = visitExpression(structAssignment.value, tempOperand)
    tempOperand match {
      case operand: IntOperand => valueBlock.append(CompoundAssignInt(operand, structOperand, memberOffset))
      case operand: FloatOperand => valueBlock.append(CompoundAssignFloat(operand, structOperand, memberOffset))
      case operand: BoolOperand => valueBlock.append(CompoundAssignBool(operand, structOperand, memberOffset))
      case operand: ReferenceOperand => valueBlock.append(CompoundAssignReference(operand, structOperand, memberOffset))
    }
  }

  protected def visitExpressionWrapper(expressionWrapper: ExpressionWrapper): CodeBlock = {
    // TODO: This is pretty lazy, should have a special unused value at the very least...
    visitExpression(expressionWrapper.expression, newTemp(expressionWrapper.expression.`type`))
  }

  protected def visitBlock(block: Block, operand: Operand): CodeBlock = {
    val statementsBlock = block.statements.map(visitStatement).foldLeft(CodeBlock())((left, right) => left.append(right))
    block.expression.foreach(expr => statementsBlock.append(visitExpression(expr, operand)))
    statementsBlock
  }

  protected def visitLambda(lambda: Lambda, operand: Operand): CodeBlock = {
    sys.error("Lambdas have not been implemented yet.")
  }

  protected def visitVariable(variable: Variable, operand: Operand): CodeBlock = {
    val name = variable.identifier
    variable.`type` match {
      // TODO: Everything about this is terrible...it should be fixed!
      case IntegralType => CodeBlock(AssignInt(operand.asInstanceOf[IntOperand], IntRealOperand(name)))
      case FloatingPointType => CodeBlock(AssignFloat(operand.asInstanceOf[FloatOperand], FloatRealOperand(name)))
      case BoolType => CodeBlock(AssignBool(operand.asInstanceOf[BoolOperand], BoolRealOperand(name)))
      case _ => CodeBlock(AssignReference(operand.asInstanceOf[ReferenceOperand], ReferenceRealOperand(name)))
    }
  }

  // TODO: We need to handle the case that there is no alternative in the if statement.
  protected def visitIfExpression(ifExpression: IfExpression, operand: Operand): CodeBlock = {
    val conditionOperand = newTempBool()
    val IfExpression(conditionExpr, bodyExpr, alternativeExpr) = ifExpression
    val conditionBlock = visitBooleanExpression(conditionExpr, conditionOperand)
    val bodyBlock = visitBlock(bodyExpr, operand)
    val alternativeBlock = visitExpression(alternativeExpr.get, operand)

    conditionBlock.append(ConditionalGotoNot(conditionOperand, alternativeBlock.blockStart))
    bodyBlock.append(Goto(alternativeBlock.blockEnd))

    CodeBlock(conditionBlock, bodyBlock, alternativeBlock)
  }

  // TODO: Operand should be optional or something since statements do not provide operands.
  protected def visitWhileExpression(whileExpression: WhileExpression, operand: Operand): CodeBlock = {
    val conditionOperand = newTempBool()
    val conditionBlock = visitBooleanExpression(whileExpression.condition, conditionOperand)

    val bodyBlock = visitBlock(whileExpression.body, operand)
    bodyBlock.append(Goto(conditionBlock.blockStart))

    conditionBlock.append(ConditionalGotoNot(conditionOperand, bodyBlock.blockEnd))

    CodeBlock(conditionBlock, bodyBlock)
  }

  protected def visitMatchExpression(matchExpression: MatchExpression, operand: Operand): CodeBlock = {
    val MatchExpression(union, cases) = matchExpression
    val reference = newTempReference()
    val selectorOperand = newTempInt()
    val block = visitExpression(union, reference)
    block.append(CompoundAccessInt(selectorOperand, reference, 0))

    val caseMap = cases.map(kase => (kase.parameter.paramType, kase)).toMap
    val members = matchExpression.unionDef.members
    val caseBlocks = members.map(caseMap).zipWithIndex
      .map { case (kase, selector) => visitCase(kase, reference, selector, operand) }
    val jumpTable = caseBlocks.zipWithIndex.map { case (codeBlock, selector) =>
      val condition = newTempBool()
      CodeBlock(
        ComparisonInt(condition, selectorOperand, EQUALS, IntConstOperand(selector)),
        ConditionalGoto(condition, codeBlock.blockStart)
      )
    }.reduce((left, right) => left.append(right))
    val finishedCaseBlock = (caseBlocks.init.map(_.append(Goto(caseBlocks.last.blockEnd))) :+ caseBlocks.last)
      .reduce((left, right) => left.append(right))
    CodeBlock(block, jumpTable, finishedCaseBlock)
  }

  protected def visitCase(caseExpression: Case, reference: ReferenceOperand, selector: Int, operand: Operand): CodeBlock = {
    val Case(param, body) = caseExpression
    val caseOperand = newRealOperand(param.identifier, param.paramType)
    val block = caseOperand match {
      case operand: IntOperand => CodeBlock(CompoundAccessInt(operand, reference, selector + 1))
      case operand: FloatOperand => CodeBlock(CompoundAccessFloat(operand, reference, selector + 1))
      case operand: BoolOperand => CodeBlock(CompoundAccessBool(operand, reference, selector + 1))
      case operand: ReferenceOperand => CodeBlock(CompoundAccessReference(operand, reference, selector + 1))
    }
    block.append(visitExpression(body, operand))
  }

  protected def visitFunctionCall(functionCall: FunctionCall, returnOperand: Operand): CodeBlock = {
    functionCall.functionDefinition match {
      case real: FunctionSymbolDefinition => visitFunctionCallReal(functionCall, real.symbol, returnOperand)
      case ptr: VariableSymbolDefinition => visitFunctionCallPtr(functionCall, ptr.symbol, returnOperand)
    }
  }

  protected def visitFunctionCallReal(functionCall: FunctionCall, functionDefinition: FunctionDefinition, returnOperand: Operand): CodeBlock = {
    val (operands, blocks) = functionCall.parameters.map { expr =>
      val operand = newTemp(expr.`type`)
      (operand, visitExpression(expr, operand))
    }.unzip
    val block = blocks.foldLeft(CodeBlock())((left, right) => left.append(right))

    operands.foreach {
      case operand: IntOperand => block.append(PushInt(operand))
      case operand: FloatOperand => block.append(PushFloat(operand))
      case operand: BoolOperand => block.append(PushBool(operand))
      case operand: ReferenceOperand => block.append(PushReference(operand))
    }

    val functionLabel = LabelFuture(() => functionLabelMap(functionDefinition))
    returnOperand match {
      case operand: IntOperand => block.append(CallFuncInt(operand, functionLabel))
      case operand: FloatOperand => block.append(CallFuncFloat(operand, functionLabel))
      case operand: BoolOperand => block.append(CallFuncBool(operand, functionLabel))
      case operand: ReferenceOperand => block.append(CallFuncReference(operand, functionLabel))
      case _ => block.append(CallFunc(functionLabel))
    }
    Pop(operands.size)
    block
  }

  protected def visitFunctionCallPtr(functionCall: FunctionCall, variableDeclaration: VariableDeclaration, returnOperand: Operand): CodeBlock = {
    val functionPtr = newRealOperand(variableDeclaration.identifier, variableDeclaration.varType.get).asInstanceOf[ReferenceOperand]

    val (operands, blocks) = functionCall.parameters.map { expr =>
      val operand = newTemp(expr.`type`)
      (operand, visitExpression(expr, operand))
    }.unzip
    val block = blocks.reduce((left, right) => left.append(right))

    operands.foreach {
      case operand: IntOperand => block.append(PushInt(operand))
      case operand: FloatOperand => block.append(PushFloat(operand))
      case operand: BoolOperand => block.append(PushBool(operand))
      case operand: ReferenceOperand => block.append(PushReference(operand))
    }

    returnOperand match {
      case operand: IntOperand => block.append(PtrCallFuncInt(operand, functionPtr))
      case operand: FloatOperand => block.append(PtrCallFuncFloat(operand, functionPtr))
      case operand: BoolOperand => block.append(PtrCallFuncBool(operand, functionPtr))
      case operand: ReferenceOperand => block.append(PtrCallFuncReference(operand, functionPtr))
      case _ => block.append(PtrCallFunc(functionPtr))
    }
    Pop(operands.size)
    block
  }

  protected def visitMethodCall(methodCall: MethodCall, operand: Operand): CodeBlock = {
    sys.error("Methods are not yet implemented.")
  }

  protected def visitStructAccess(structAccess: StructAccess, returnOperand: Operand): CodeBlock = {
    val structOperand = newTempReference()
    val structBlock = visitExpression(structAccess.receiver, structOperand)
    val memberOffset = structAccess.structDef.fields.map(_.identifier).indexOf(structAccess.identifier)
    returnOperand match {
      case operand: IntOperand => structBlock.append(CompoundAccessInt(operand, structOperand, memberOffset))
      case operand: FloatOperand => structBlock.append(CompoundAccessFloat(operand, structOperand, memberOffset))
      case operand: BoolOperand => structBlock.append(CompoundAccessBool(operand, structOperand, memberOffset))
      case operand: ReferenceOperand => structBlock.append(CompoundAccessReference(operand, structOperand, memberOffset))
    }
  }

  protected def visitThis(operand: Operand): CodeBlock = {
    sys.error("Methods are not yet implemented.")
  }

  protected def visitStructConstructor(structConstructor: StructConstructor, returnOperand: ReferenceOperand): CodeBlock = {
    val operands = structConstructor.definition.fields.map(field => newTemp(field.fieldType))
    val block = CodeBlock(HeapAllocateStruct(returnOperand, operands))
    structConstructor.params.map(param => newRealOperand(param.identifier, param.`type`)).zipWithIndex.foreach {
      case (operand: IntOperand, offset) => block.append(CompoundAssignInt(operand, returnOperand, offset))
      case (operand: FloatOperand, offset) => block.append(CompoundAssignFloat(operand, returnOperand, offset))
      case (operand: BoolOperand, offset) => block.append(CompoundAssignBool(operand, returnOperand, offset))
      case (operand: ReferenceOperand, offset) => block.append(CompoundAssignReference(operand, returnOperand, offset))
    }
    block
  }

  protected def visitUnionConstructor(unionConstructor: UnionConstructor, returnOperand: ReferenceOperand): CodeBlock = {
    val operands = unionConstructor.definition.members.map(newTemp)
    val selector = unionConstructor.definition.members.indexOf(unionConstructor.param.`type`)
    val block = CodeBlock(HeapAllocateUnion(returnOperand, selector, operands))
    val paramOperand = newRealOperand(unionConstructor.param.identifier, unionConstructor.param.`type`)
    paramOperand match {
      case operand: IntOperand => block.append(CompoundAssignInt(operand, returnOperand, selector + 1))
      case operand: FloatOperand => block.append(CompoundAssignFloat(operand, returnOperand, selector + 1))
      case operand: BoolOperand => block.append(CompoundAssignBool(operand, returnOperand, selector + 1))
      case operand: ReferenceOperand => block.append(CompoundAssignReference(operand, returnOperand, selector + 1))
    }
  }

  protected def visitPrintExpression(printExpression: PrintExpression): CodeBlock = {
    val paramOperand = newRealOperand(printExpression.value.identifier, printExpression.value.`type`)
    CodeBlock(Print(paramOperand))
  }

  protected def visitBooleanBinaryExpression(booleanBinaryExpression: BooleanBinaryExpression, operand: BoolOperand): CodeBlock = {
    val BooleanBinaryExpression(leftExpression, op, rightExpression) = booleanBinaryExpression
    val leftOperand = newTempBool()
    val rightOperand = newTempBool()

    val leftBlock = visitBooleanExpression(leftExpression, leftOperand)
    val rightBlock = visitBooleanExpression(rightExpression, rightOperand)
    val instruction = BinaryOperationBool(operand, leftOperand, op, rightOperand)
    CodeBlock()
      .append(leftBlock)
      .append(rightBlock)
      .append(instruction)
  }

  protected def visitBooleanInverse(booleanInverse: BooleanInverse, operand: BoolOperand): CodeBlock = {
    val inner = newTempBool()
    val block = visitBooleanExpression(booleanInverse.value, inner)
    block.append(Not(operand, inner))
  }

  protected def visitBooleanComparison(booleanComparison: BooleanComparison, operand: BoolOperand): CodeBlock = {
    val BooleanComparison(leftExpr, op, rightExpr) = booleanComparison
    val leftOperand = newTemp(leftExpr.`type`).asInstanceOf[ArithmeticOperand]
    val rightOperand = newTemp(rightExpr.`type`).asInstanceOf[ArithmeticOperand]

    val leftBlock = visitArithmeticExpression(leftExpr, leftOperand)
    val rightBlock = visitArithmeticExpression(rightExpr, rightOperand)
    val instruction = buildComparison(operand, leftOperand, op, rightOperand)
    CodeBlock()
      .append(leftBlock)
      .append(rightBlock)
      .append(instruction)
  }

  protected def buildComparison(result: BoolOperand,
                                left: ArithmeticOperand,
                                op: ComparisonOperator,
                                right: ArithmeticOperand): CodeBlock = {
    (left, right) match {
      case (l: IntOperand, r: IntOperand) => CodeBlock(ComparisonInt(result, l, op, r))
      case (l: IntOperand, r: FloatOperand) =>
        val newL = newTempFloat()
        CodeBlock()
          .append(ToFloat(newL, l))
          .append(ComparisonFloat(result, newL, op, r))
      case (l: FloatOperand, r: IntOperand) =>
        val newR = newTempFloat()
        CodeBlock()
          .append(ToFloat(newR, r))
          .append(ComparisonFloat(result, l, op, newR))
      case (l: FloatOperand, r: FloatOperand) => CodeBlock(ComparisonFloat(result, l, op, r))
    }
  }

  protected def visitBooleanExpressionWrapper(booleanExpressionWrapper: BooleanExpressionWrapper, operand: BoolOperand): CodeBlock = {
    visitExpression(booleanExpressionWrapper.expression, operand)
  }

  protected def visitBooleanConstant(booleanConstant: BooleanConstant, operand: BoolOperand): CodeBlock = {
    CodeBlock(AssignBool(operand, BoolConstOperand(booleanConstant.value)))
  }

  protected def visitArithmeticBinaryExpression[T <: ArithmeticOperand](arithmeticBinaryExpression: ArithmeticBinaryExpression, operand: T): CodeBlock = {
    val ArithmeticBinaryExpression(leftExpr, op, rightExpr) = arithmeticBinaryExpression
    operand match {
      case intOperand: IntOperand =>
        val leftOperand = newTempInt()
        val rightOperand = newTempInt()
        CodeBlock()
          .append(visitArithmeticExpression(leftExpr, leftOperand))
          .append(visitArithmeticExpression(rightExpr, rightOperand))
          .append(BinaryOperationInt(intOperand, leftOperand, op, rightOperand))
      case floatOperand: FloatOperand =>
        val leftOperand = newTempFloat()
        val rightOperand = newTempFloat()
        CodeBlock()
          .append(visitArithmeticExpression(leftExpr, leftOperand))
          .append(visitArithmeticExpression(rightExpr, rightOperand))
          .append(BinaryOperationFloat(floatOperand, leftOperand, op, rightOperand))
    }
  }

  protected def visitArithmeticExpressionWrapper[T <: ArithmeticOperand](wrapper: ArithmeticExpressionWrapper, operand: T): CodeBlock = {
    operand match {
      case result: IntOperand => visitExpression(wrapper.expression, result)
      case result: FloatOperand => wrapper.expression.`type` match {
        case FloatingPointType => visitExpression(wrapper.expression, result)
        case IntegralType =>
          val intOperand = newTempInt()
          CodeBlock(ToFloat(result, intOperand))
            .append(visitExpression(wrapper.expression, result))
        case _ => sys.error("Where is your god now?")
      }
    }
  }

  protected def visitArithmeticIntegralConstant[T <: ArithmeticOperand](arithmeticIntegralConstant: ArithmeticIntegralConstant, operand: T): CodeBlock = {
    operand match {
      case result: IntOperand => CodeBlock(AssignInt(result, IntConstOperand(arithmeticIntegralConstant.value)))
      case result: FloatOperand => CodeBlock(ToFloat(result, IntConstOperand(arithmeticIntegralConstant.value)))
    }
  }

  protected def visitArithmeticFloatingPointConstant[T <: ArithmeticOperand](arithmeticFloatingPointConstant: ArithmeticFloatingPointConstant, operand: T): CodeBlock = {
    operand match {
      case result: FloatOperand => CodeBlock(AssignFloat(result, FloatConstOperand(arithmeticFloatingPointConstant.value)))
    }
  }
}