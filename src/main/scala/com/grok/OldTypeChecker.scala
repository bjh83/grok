package com.grok

/**
 * Created by brendan on 3/28/15.
 */
class OldTypeChecker {
  var definitionTable = new DefinitionTable
  var conversionTable = new ConversionTable(definitionTable)

  val visitTopLevelStatement: PartialFunction[TopLevelStatement, Unit] = {
    case functionDefinition: FunctionDefinition =>
    case methodDefinition: MethodDefinition => sys.error("Not yet implemented")
    case structDefinition: StructDefinition =>
    case unionDefinition: UnionDefinition =>
    case interfaceDefinition: InterfaceDefinition =>
    case instance: Instance => sys.error("Not yet implemented")
    case statement: Statement => visitStatement(statement)
  }

  val visitStatement: PartialFunction[Statement, Unit] = {
    case variableDeclaration: VariableDeclaration => visitVariableDeclaration(variableDeclaration)
    case variableAssignment: VariableAssignment => visitVariableAssignment(variableAssignment)
    case structAssignment: StructAssignment => visitStructAssignment(structAssignment)
    case ExpressionWrapper(expr) => visitExpression(expr)
  }

  def visitVariableDeclaration(variableDeclaration: VariableDeclaration): Type = {
    val VariableDeclaration(mutability, identifier, varType, value) = variableDeclaration
    val variableType = visitExpression(value)
    varType match {
      case Some(typ) => if (typ == variableType) {
        definitionTable.updateSymbol(VariableDeclaration(mutability, identifier, Some(variableType), value))
        UnitType
      } else {
        sys.error("Declare type, " + typ + ", and actual type, " + variableType + ", do not match.")
      }
      case None =>
        definitionTable.updateSymbol(VariableDeclaration(mutability, identifier, Some(variableType), value))
        UnitType
    }
  }

  def visitStructAssignment(structAssignment: StructAssignment): Type = {
    definitionTable.lookupSymbol(VariableKey(structAssignment.identifier)).`type`() match {
      case simpleType: SimpleType => definitionTable.lookupDefinition(simpleType) match {
        case structDefinition: StructDefinition => structDefinition.fields.find(_.equals(structAssignment.member)) match {
          case Some(_) => UnitType
          case None => sys.error("Struct does not contain field: " + structAssignment.member)
        }
        case _ => sys.error("No struct definition named: " + structAssignment.identifier)
      }
      case _ => sys.error("No struct definition name: " + structAssignment.identifier)
    }
  }

  def visitVariableAssignment(variableAssignment: VariableAssignment): Type = {
    val oldType = definitionTable.lookupSymbol(VariableKey(variableAssignment.identifier)).`type`()
    val newType = visitExpression(variableAssignment.value)
    if (oldType != newType) {
      sys.error("Declared type, " + oldType + ", does not match expression type, " + newType)
    }
    UnitType
  }

  val visitExpression: PartialFunction[Expression, Type] = {
    case block: Block => visitBlock(block)
    case lambda: Lambda => sys.error("Not yet implemented")
    case booleanExpression: BooleanExpression => visitBooleanExpression(booleanExpression)
    case arithmeticExpression: ArithmeticExpression => visitArithmeticExpression(arithmeticExpression)
    case Variable(variable) => definitionTable.lookupSymbol(VariableKey(variable)).`type`()
    case IfExpression(condition, body, alternative) => visitIfExpression(condition, body, alternative)
    case WhileExpression(condition, body) => visitWhileExpression(condition, body)
    case MatchExpression(expression, cases) => visitMatchExpression(expression, cases)
    case functionCall: FunctionCall => visitFunctionCall(functionCall)
    case MethodCall(receiver, identifier, params) => sys.error("Not yet implemented")
    case StructAccess(receiver, identifier) => visitStructAccess(receiver, identifier)
    case This => sys.error("Not yet implemented")
  }

  def visitBlock(block: Block): Type = {
    definitionTable.push()
    block.statements.foreach(visitStatement)
    block.expression.map(visitExpression) match {
      case Some(typ) => definitionTable.pop(); typ
      case None => definitionTable.pop(); UnitType
    }
  }

  val visitBooleanExpression: PartialFunction[BooleanExpression, Type] = {
    case BooleanBinaryExpression(left, _, right) => visitBooleanExpression(left); visitBooleanExpression(right)
    case BooleanInverse(value) => visitBooleanExpression(value)
    case BooleanComparison(left, _, right) => visitArithmeticExpression(left); visitArithmeticExpression(right); BoolType
    case BooleanExpressionWrapper(value) => visitExpression(value)
    case _: BooleanConstant => BoolType
  }

  val visitArithmeticExpression: PartialFunction[ArithmeticExpression, Type] = {
    case ArithmeticBinaryExpression(left, _, right) => visitArithmeticExpression(left); visitArithmeticExpression(right)
    case ArithmeticExpressionWrapper(expr) => visitExpression(expr)
    case ArithmeticIntegralConstant(_) => IntegralType
    case ArithmeticFloatingPointConstant(_) => FloatingPointType
  }

  def visitIfExpression(condition: BooleanExpression, body: Block, alternative: Option[Expression]): Type = {
    visitBooleanExpression(condition)
    visitBlock(body)
    alternative.map(visitExpression) match {
      case Some(typ) => typ
      case None => UnitType
    }
  }

  private def visitWhileExpression(condition: BooleanExpression, body: Block): Type = {
    visitBooleanExpression(condition)
    visitBlock(body)
  }

  private def visitMatchExpression(expression: Expression, cases: List[Case]): Type = {
    visitExpression(expression)
    val types = cases.map(kase => visitExpression(kase.body))
    conversionTable.commonDerivedType(types)
  }

  private def visitFunctionCall(functionCall: FunctionCall): Type = {
    val FunctionCall(identifier, params) = functionCall
    val funcDef = definitionTable.lookupSymbol(PartialFunctionKey(identifier, params.map(visitExpression)))
      .asInstanceOf[FunctionDefinition]
    functionCall.definition = funcDef
    funcDef.returnType
  }

  private def visitStructAccess(receiver: Expression, member: String): Type = {
    visitExpression(receiver) match {
      case simpleType: SimpleType => definitionTable.lookupDefinition(simpleType) match {
        case structDef: StructDefinition => structDef.fields.find(_.identifier == member) match {
          case Some(Field(_, typ, _)) => typ
          case None => sys.error("Struct, " + structDef.identifier + ", has no field named, " + member)
        }
        case result => sys.error("No struct type named: " + result)
      }
      case result => sys.error("No struct type named: " + result)
    }
  }
}