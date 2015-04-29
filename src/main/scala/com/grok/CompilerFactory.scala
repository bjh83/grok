package com.grok

import java.io.{FileInputStream, File}

import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ANTLRInputStream

import scala.collection.mutable

/**
 * Created by brendan.
 */
object CompilerFactory {
  def compiler(flags: Flags): Compiler = {
    flags.compilerType match {
      case DEFAULT_COMPILER => new DefaultCompiler
      case TAC => new DefaultCompiler
      case INTERPRETER => new Interpreter
      case VM_COMPILER => new VMCompiler
      case _ => sys.error("Compiler type unsupported.")
    }
  }
}

abstract class Compiler {
  def compile(sources: Seq[String]): Unit
}

sealed abstract class CompilerType
case object DEFAULT_COMPILER extends CompilerType
case object INTERPRETER extends CompilerType
case object VM_COMPILER extends CompilerType
case object TAC extends CompilerType

class DefaultCompiler extends Compiler {
  private val buildAST = (new ASTBuilder).visit _

  override def compile(sources: Seq[String]): Unit = {
    if (sources.isEmpty) {
      sys.error("Must provide at least one source file.")
    }
    sources.foreach(source => compileSource(new File(source)))
  }

  protected def compileSource(file: File): Unit = {
    val ast = mutable.MutableList(compileToAST(new ANTLRInputStream(new FileInputStream(file))): _*)
    val (symbolTable, builtInFunctions) = buildSymbolTable(ast)
    typeCheck(ast, symbolTable)
    val (code, functionTable) = generateIntermediateCode(ast ++ builtInFunctions)
    handleIntermediateCode(code, functionTable)
  }

  protected def compileToAST(inputStream: ANTLRInputStream): List[TopLevelStatement] = {
    val lexer = new GrokLexer(inputStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new GrokParser(tokens)
    buildAST(parser.compilationUnit())
  }

  protected def buildSymbolTable(ast: mutable.MutableList[TopLevelStatement]): (InitialDefinitionTable, List[FunctionDefinition]) = {
    (new SemanticAnalyzer).visitAST(ast)
  }

  protected def typeCheck(ast: mutable.MutableList[TopLevelStatement], symbolTable: InitialDefinitionTable): Unit = {
    val unionDefinitions = symbolTable.definitions.values.filter(_.isInstanceOf[UnionDefinition]).toSet.asInstanceOf[Set[UnionDefinition]]
    val structDefinitions = symbolTable.definitions.values.filter(_.isInstanceOf[StructDefinition]).toSet.asInstanceOf[Set[StructDefinition]]
    val typeTable = new TypeTableFactory(unionDefinitions, structDefinitions).build()
    (new TypeChecker).visitAST(ast, symbolTable.buildFinalDefinitionTable(), typeTable)
  }

  protected def generateIntermediateCode(ast: mutable.MutableList[TopLevelStatement]): (CodeBlock, Map[String, Label]) = {
    (new CodeGenerator).visitAST(ast)
  }

  protected def handleIntermediateCode(code: CodeBlock, functionTable: Map[String, Label]): Unit = {
    functionTable.foreach { case (name, label) => println(name + " = " + label.toRealizedLabel)}
    println(code)
  }
}

class VMCompiler extends DefaultCompiler {
  override protected def handleIntermediateCode(code: CodeBlock, functionTable: Map[String, Label]): Unit = {
    val newFunctionTable = functionTable.map { case (name, label) =>
      (ReferenceRealOperand(name), ReferenceOperandValue(label.toRealizedLabel.index))
    }
    new VirtualMachine(code.toList, newFunctionTable).execute()
  }
}