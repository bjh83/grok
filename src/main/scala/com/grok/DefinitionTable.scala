package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
abstract class DefinitionTable {
  val definitions = mutable.Map[String, TypeDefinition]()
  val symbolTable = mutable.Stack[mutable.Map[String, SymbolGroup[_, Key, _]]]()
  push()

  def addDefinition(typeDefinition: TypeDefinition): Unit = {
    val name: String = typeDefinition match {
      case definition: StructDefinition => definition.identifier
      case definition: UnionDefinition => definition.identifier
      case definition: InterfaceDefinition => definition.identifier
      // TODO: This is terrible!
      case definition: Instance => definition.implementer.toString
    }
    if (definitions.contains(name)) {
      sys.error("Definition has already been defined: " + typeDefinition)
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

  def push(): Unit = symbolTable.push(mutable.HashMap[String, SymbolGroup[_, Key, _]]())

  def pop(): Unit = symbolTable.pop()

  def addSymbol(definition: SymbolDefinition[_, Key]): Unit = {
    val name: String = definition.key.name
    val currentTable = symbolTable.head
    if (currentTable.contains(name)) {
      currentTable(name).add(definition)
    } else {
      currentTable(name) = definition.buildSymbolGroup().asInstanceOf[SymbolGroup[_, Key, _]]
    }
  }

  def containsSymbol(key: Key): Boolean = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(_) => true
      case None => false
    }
  }

  def lookupSymbolGroup(key: Key): SymbolGroup[_, Key, SymbolDefinition[_, Key]] = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(table) => table(key.name).asInstanceOf[SymbolGroup[_, Key, SymbolDefinition[_, Key]]]
      case None => sys.error("No symbol, " + key.name + ", was declared")
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
    val newSymbolTable: mutable.Stack[mutable.Map[String, SymbolGroup[_, Key, _]]] =
      symbolTable.map(frame => mutable.Map(frame.mapValues(_.buildWithTable(typeTable)).toSeq: _*))
        .asInstanceOf[mutable.Stack[mutable.Map[String, SymbolGroup[_, Key, _]]]]
    new FinalDefinitionTable(definitions, newSymbolTable, typeTable)
  }
}

class FinalDefinitionTable(override val definitions: mutable.Map[String, TypeDefinition],
                           override val symbolTable: mutable.Stack[mutable.Map[String, SymbolGroup[_, Key, _]]],
                           val typeTable: TypeTable)
  extends DefinitionTable {

  def updateSymbol(definition: SymbolDefinition[_, Key]): Unit = {
    symbolTable.find(_.contains(definition.key.name)) match {
      case Some(table) => table(definition.key.name).update(definition)
      case None => sys.error("No symbol, " + definition.key.name + ", was declared")
    }
  }

  def lookupSymbol(key: Key): SymbolDefinition[_, Key] = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(table) => table(key.name).get(key)
      case None => sys.error("No symbol, " + key.name + ", was declared")
    }
  }
}