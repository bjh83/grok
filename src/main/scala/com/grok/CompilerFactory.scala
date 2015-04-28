package com.grok

import java.io.{FileInputStream, File}

import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ANTLRInputStream

/**
 * Created by brendan.
 */
object CompilerFactory {
  def compiler(flags: Flags): Compiler = if (flags.compilerType == DEFAULT_COMPILER) {
    new DefaultCompiler
  } else if (flags.compilerType == INTERPRETER) {
    new Interpreter
  } else if (flags.compilerType == VM_COMPILER) {
    new VMCompiler
  } else {
    sys.error("Compiler type unsupported.")
  }
}

abstract class Compiler {
  def compile(sources: Seq[String]): Unit
}

sealed abstract class CompilerType
case object DEFAULT_COMPILER extends CompilerType
case object INTERPRETER extends CompilerType
case object VM_COMPILER extends CompilerType

class DefaultCompiler extends Compiler {
  private val buildAST = (new ASTBuilder).visit _

  override def compile(sources: Seq[String]): Unit = {
    if (sources.isEmpty) {
      sys.error("Must provide at least one source file.")
    }
    sources.foreach(source => compileSource(new File(source)))
  }

  protected def compileSource(file: File): Unit = {
    val ast = compileToAST(new ANTLRInputStream(new FileInputStream(file)))
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

  protected def buildSymbolTable(ast: List[TopLevelStatement]): (InitialDefinitionTable, List[FunctionDefinition]) = {
    (new SemanticAnalyzer).visitAST(ast)
  }

  protected def typeCheck(ast: List[TopLevelStatement], symbolTable: InitialDefinitionTable): Unit = {
    val unionDefinitions = symbolTable.definitions.values.filter(_.isInstanceOf[UnionDefinition]).toSet.asInstanceOf[Set[UnionDefinition]]
    val structDefinitions = symbolTable.definitions.values.filter(_.isInstanceOf[StructDefinition]).toSet.asInstanceOf[Set[StructDefinition]]
    val typeTable = new TypeTableFactory(unionDefinitions, structDefinitions).build()
    (new TypeChecker).visitAST(ast, symbolTable.buildFinalDefinitionTable(), typeTable)
  }

  protected def generateIntermediateCode(ast: List[TopLevelStatement]): (CodeBlock, Map[String, Label]) = {
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