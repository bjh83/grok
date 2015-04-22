package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
abstract class DefinitionTable {
  val definitions = mutable.HashMap[String, TypeDefinition]()
  val symbolTable = mutable.Stack[mutable.Map[String, SymbolGroup]]()
  push()

  def addDefinition(typeDefinition: TypeDefinition): Unit = {
    val name = typeDefinition.key().name
    if (definitions.contains(name)) {
      sys.error("Definition has already been defined: " + typeDefinition.identifier)
    }
    definitions(name) = typeDefinition
  }

  def lookupDefinition(`type`: Type): TypeDefinition = {
    val simpleType = `type` match {
      case simple: SimpleType => simple
      case _ => sys.error("Type, " + `type` + ", is not a user defined type.")
    }

    val name = simpleType.identifier
    if (!definitions.contains(name)) {
      sys.error("Type has not been defined: " + name)
    }
    definitions(name)
  }

  def lookupStructDefinition(`type`: Type): StructDefinition = {
    lookupDefinition(`type`) match {
      case struct: StructDefinition => struct
      case _ => sys.error("Type, " + `type` + ", is not a struct type.")
    }
  }

  def lookupUnionDefinition(`type`: Type): UnionDefinition = {
    lookupDefinition(`type`) match {
      case union: UnionDefinition => union
      case _ => sys.error("Type, " + `type` + ", is not a union type.")
    }
  }

  def push(): Unit = symbolTable.push(mutable.HashMap[String, SymbolGroup]())

  def pop(): Unit = symbolTable.pop()

  def addSymbol(definition: SymbolDefinition): Unit = {
    val name = definition.key.name
    val currentTable = symbolTable.head
    if (currentTable.contains(name)) {
      currentTable(name).add(definition)
    } else {
      currentTable(name) = definition.buildSymbolGroup()
    }
  }

  def containsSymbol(key: Key): Boolean = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(_) => true
      case None => false
    }
  }

  def containsSymbolFail(key: Key): Unit = if (!containsSymbol(key)) {
    sys.error("Does not contain symbol: " + key.name)
  }
}

class InitialDefinitionTable extends DefinitionTable {
  def buildFinalDefinitionTable(): FinalDefinitionTable = {
    val typeDefinitions = definitions.values.toSet
    val structs = typeDefinitions.filter {
      case struct: StructDefinition => true
      case _ => false
    }.asInstanceOf[Set[StructDefinition]]
    val unions = typeDefinitions.filter {
      case union: UnionDefinition => true
      case _ => false
    }.asInstanceOf[Set[UnionDefinition]]
    val typeTable = new TypeTableFactory(unions, structs).build()
    val newSymbolTable = symbolTable.map(frame => mutable.Map(frame.mapValues(_.buildWithTable(typeTable)).toSeq: _*))
    new FinalDefinitionTable(definitions, newSymbolTable, typeTable)
  }
}

class FinalDefinitionTable(override protected val definitions: mutable.Map[String, TypeDefinition],
                           override protected val symbolTable: mutable.Stack[mutable.Map[String, SymbolGroup]],
                           val typeTable: TypeTable)
  extends DefinitionTable {

  def updateSymbol(definition: SymbolDefinition): Unit = {
    symbolTable.find(_.contains(definition.key.name)) match {
      case Some(table) => table(definition.key.name).update(definition)
      case None => sys.error("No symbol, " + definition.key.name + ", was declared")
    }
  }

  def lookupSymbol(key: Key): SymbolDefinition = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(table) => table(key.name).get(key)
      case None => sys.error("No symbol, " + key.name + ", was declared")
    }
  }
}