package com.grok

import java.io.{FileInputStream, File}

import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}

import scala.collection.mutable

/**
 * Created by brendan on 3/6/15.
 */
class Interpreter extends Compiler {
  private val buildAST = (new ASTBuilder).visit _
  val executionContext = new ExecutionContext

  override def compile(sources: Seq[String]): Unit = {
    if (sources.isEmpty) {
      sys.error("Must provide at least one source file.")
    }
    sources.foreach(source => compileSource(new File(source)))
  }

  private def compileSource(file: File): Unit = {
    val compilationUnit = compileToAST(new ANTLRInputStream(new FileInputStream(file)))
    executeAST(compilationUnit)
  }

  private def compileToAST(inputStream: ANTLRInputStream): List[TopLevelStatement] = {
    val lexer = new GrokLexer(inputStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new GrokParser(tokens)
    buildAST(parser.compilationUnit())
  }

  def executeAST(ast: List[TopLevelStatement]): Unit = {
    ast.foreach(executeTopLevelStatement)
  }

  private val executeTopLevelStatement: PartialFunction[TopLevelStatement, Unit] = {
    case functionDefinition: FunctionDefinition => executionContext.functionDefinitions(functionDefinition.identifier) = functionDefinition
    case methodDefinition: MethodDefinition => executionContext.methodDefinitions(methodDefinition.identifier) = methodDefinition
    case structDefinition: StructDefinition => executionContext.structDefinitions(structDefinition.identifier) = structDefinition
    case unionDefinition: UnionDefinition => executionContext.unionDefinitions(unionDefinition.identifier) = unionDefinition
    case interfaceDefinition: InterfaceDefinition => executionContext.interfaceDefinitions(interfaceDefinition.identifier) = interfaceDefinition
    case instance: Instance => executionContext.addInstance(instance)
    case statement: Statement => executeStatement(statement)
  }

  private val executeStatement: PartialFunction[Statement, Unit] = {
    case variableDeclaration: VariableDeclaration => executionContext.declare(variableDeclaration.identifier, evaluateExpression(variableDeclaration.value))
    case variableAssignment: VariableAssignment => executionContext.assign(variableAssignment.identifier, evaluateExpression(variableAssignment.value))
    case structAssignment: StructAssignment => executionContext.lookup(structAssignment.identifier).asInstanceOf[mutable.HashMap[String, Any]](structAssignment.member) = evaluateExpression(structAssignment.value)
    case ExpressionWrapper(expression) => evaluateExpression(expression)
  }

  private val evaluateExpression: PartialFunction[Expression, Any] = {
    case block: Block => executeBlock(block)
    case lambda: Lambda => lambda
    case booleanExpression: BooleanExpression => evaluateBooleanExpression(booleanExpression)
    case arithmeticExpression: ArithmeticExpression => evaluateArithmeticExpression(arithmeticExpression)
    case Variable(identifier) => executionContext.lookup(identifier)
    case IfExpression(condition, body, alternative) => evaluateIfExpression(condition, body, alternative)
    case WhileExpression(condition, body) => while (evaluateBooleanExpression(condition)) { evaluateExpression(body) }
    case MatchExpression(expression, cases) => sys.error("Match expression is not supported.")
    case FunctionCall(identifier, parameters) => executeFunction(identifier, parameters.map(evaluateExpression))
    case MethodCall(receiver, identifier, parameters) => executeMethod(evaluateExpression(receiver), identifier, parameters.map(evaluateExpression))
    case StructAccess(receiver, member) => evaluateExpression(receiver).asInstanceOf[mutable.HashMap[String, Any]](member)
    case This => executionContext.lookup("this")
  }

  private def evaluateIfExpression(condition: BooleanExpression, body: Block, alternative: Option[Expression]): Any = {
    if (evaluateBooleanExpression(condition)) {
      evaluateExpression(body)
    } else {
      val result = alternative.map(evaluateExpression).getOrElse(None)
      if (result == None) {
        println("WARNING: Alternative evaluated to None.")
      }
      result
    }
  }

  private val evaluateBooleanExpression: PartialFunction[BooleanExpression, Boolean] = {
    case BooleanBinaryExpression(left, op, right) => performBooleanOperation(evaluateBooleanExpression(left), op, evaluateBooleanExpression(right))
    case BooleanInverse(value) => !evaluateBooleanExpression(value)
    case BooleanComparison(left, op, right) => compare(evaluateArithmeticExpression(left), op, evaluateArithmeticExpression(right))
    case BooleanExpressionWrapper(expr) => evaluateExpression(expr).asInstanceOf[Boolean]
    case BooleanConstant(value) => value
  }

  private def performBooleanOperation(left: Boolean, op: BooleanOperator, right: Boolean): Boolean = op match {
    case OR => left || right
    case AND => left && right
  }

  private def compare(left: Double, op: ComparisonOperator, right: Double): Boolean = op match {
    case EQUALS => left == right
    case NOT_EQUALS => left != right
    case LESS => left < right
    case GREATER => left > right
    case LESS_OR_EQUAL => left <= right
    case GREATER_OR_EQUAL => left >= right
  }

  private val evaluateArithmeticExpression: PartialFunction[ArithmeticExpression, Double] = {
    case ArithmeticBinaryExpression(left, op, right) => performArithmeticOperation(evaluateArithmeticExpression(left), op, evaluateArithmeticExpression(right))
    case ArithmeticExpressionWrapper(expr) => evaluateExpression(expr).asInstanceOf[Double]
    case ArithmeticIntegralConstant(const) => const
    case ArithmeticFloatingPointConstant(const) => const
  }

  private def performArithmeticOperation(left: Double, op: ArithmeticOperator, right: Double): Double = op match {
    case PLUS => left + right
    case MINUS => left - right
    case MULTIPLY => left * right
    case DIVIDE => left / right
    case MODULUS => left % right
  }

  private def executeFunction(identifier: String, parameters: List[Any]): Any = {
    val specialFunction = lookupSpecialFunction(identifier, parameters)
    if (specialFunction.nonEmpty) {
      specialFunction.get(parameters)
    } else {
      val FunctionDefinition(_, _, mappedParameters, _, definition) = executionContext.functionDefinitions(identifier)
      executionContext.push()
      mappedParameters.map(_.identifier).zip(parameters).foreach { case (identifier, value) => executionContext.declare(identifier, value)}
      val result = evaluateExpression(definition)
      executionContext.pop()
      result
    }
  }

  private def lookupSpecialFunction(identifier: String, parameters: List[Any]): Option[List[Any] => Any] = {
    if (identifier == "println") {
      Some({
        params: List[Any] => println(params.map(_.toString).fold("")((left, right) => left + " " + right))
      })
    } else {
      None
    }
  }

  private def executeMethod(receiver: Any, identifier: String, parameters: List[Any]): Any = {
    val MethodDefinition(_, _, _, mappedParameters, _, definition) = executionContext.methodDefinitions(identifier)
    executionContext.push()
    mappedParameters.map(_.identifier).zip(parameters).foreach { case (identifier, value) => executionContext.declare(identifier, value)}
    executionContext.declare("this", receiver)
    val result = evaluateExpression(definition)
    executionContext.pop()
    result
  }

  private def executeBlock(block: Block): Any = {
    val Block(statements, expression) = block
    executionContext.push()
    statements.foreach(executeStatement(_))
    val result = expression.map(evaluateExpression).getOrElse(None)
    executionContext.pop()
    if (result == None) {
      println("WARNING: Block evaluated to None.")
    }
    result
  }
}

class ExecutionContext {
  val functionDefinitions = mutable.HashMap[String, FunctionDefinition]()
  val methodDefinitions = mutable.HashMap[String, MethodDefinition]()
  val structDefinitions = mutable.HashMap[String, StructDefinition]()
  val unionDefinitions = mutable.HashMap[String, UnionDefinition]()
  val interfaceDefinitions = mutable.HashMap[String, InterfaceDefinition]()

  type StackFrame = mutable.HashMap[String, Any]

  val context = mutable.Stack[StackFrame]()

  def push(): Unit = context.push(new StackFrame)

  def pop(): Unit = context.pop()

  def declare(identifier: String, value: Any): Unit = {
    val currentFrame = context.head
    if (!currentFrame.contains(identifier)) {
      currentFrame(identifier) = value
    } else {
      sys.error(identifier + " was already declared.")
    }
  }

  def assign(identifier: String, value: Any): Unit = {
    context.filter(_.contains(identifier)).head(identifier) = value
  }

  def lookup(identifier: String): Any = context.filter(_.contains(identifier)).head(identifier)

  def addInstance(instance: Instance): Unit = {
    instance.members.foreach(method => methodDefinitions(method.identifier) = method)
  }
}