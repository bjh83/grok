package com.grok

import java.io.{FileInputStream, File}

import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ANTLRInputStream

/**
 * Created by brendan on 3/4/15.
 */
object CompilerFactory {
  def compiler(flags: Flags): Compiler = if (flags.compilerType == DEFAULT_COMPILER) {
    new DefaultCompiler
  } else if (flags.compilerType == INTERPRETER) {
    new Interpreter
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

class DefaultCompiler extends Compiler {
  private val buildAST = (new ASTBuilder).visit _

  override def compile(sources: Seq[String]): Unit = {
    if (sources.isEmpty) {
      sys.error("Must provide at least one source file.")
    }
    sources.foreach(source => compileSource(new File(source)))
  }

  private def compileSource(file: File): Unit = {
    val ast = compileToAST(new ANTLRInputStream(new FileInputStream(file)))
    val compilationUnit = buildSymbolTable(ast)
    compilationUnit.topLevelStatements.foreach(println)
  }

  private def compileToAST(inputStream: ANTLRInputStream): List[TopLevelStatement] = {
    val lexer = new GrokLexer(inputStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new GrokParser(tokens)
    buildAST(parser.compilationUnit())
  }

  private def buildSymbolTable(ast: List[TopLevelStatement]): CompilationUnit = {
    (new SemanticAnalyzer).analyze(ast)
  }
}