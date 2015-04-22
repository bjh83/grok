package com.grok

/**
 * Created by brendan.
 */
sealed abstract class CompilationUnit[D <: DefinitionTable] {
  def ast: List[TopLevelStatement]
  def definitionTable: D
}

case class CompilationUnitWithoutTable(ast: List[TopLevelStatement]) extends CompilationUnit[InitialDefinitionTable] {
  def definitionTable = new InitialDefinitionTable
}

case class CompilationUnitWithIncompleteTable(ast: List)