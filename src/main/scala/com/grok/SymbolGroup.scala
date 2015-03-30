package com.grok

import scala.collection.mutable

/**
 * Created by brendan on 3/28/15.
 */

abstract class Key {
  def name: String
}

case class DefinitionKey(name: String) extends Key
case class VariableKey(name: String) extends Key
abstract class FunctionKey extends Key {
  def parameters: List[Type]
}
case class PartialFunctionKey(name: String, parameters: List[Type]) extends FunctionKey
case class FullFunctionKey(name: String, returnType: Type, parameters: List[Type]) extends FunctionKey

sealed abstract class SymbolGroup

class VariableGroup(val variableDeclaration: VariableDeclaration) extends SymbolGroup

class ParameterGroup(val parameter: Parameter) extends SymbolGroup

class FunctionGroup(val returnType: Type) extends SymbolGroup {
  val functionTable = mutable.HashMap[List[Type], FunctionDefinition]()

  def addFunction(function: FunctionDefinition): Unit = {
    val FullFunctionKey(name, returnType: Type, parameters: List[Type]) = function.key()
    if (returnType != this.returnType) {
      sys.error("Functions of the same name must have the same return type: " + returnType + " vs " + this.returnType)
    }
    if (functionTable.contains(parameters)) {
      sys.error("Function has already been declared: " + name + "(" + parameters + ")")
    }
    functionTable(parameters) = function
  }

  def lookupFunction(functionKey: FunctionKey): FunctionDefinition = {
    val name = functionKey.name
    val parameters = functionKey.parameters
    if (functionKey.isInstanceOf[FullFunctionKey] && returnType != functionKey.asInstanceOf[FullFunctionKey].returnType) {
      sys.error("Functions of the same name must have the same return type: " + returnType + " vs " + this.returnType)
    }
    if (!functionTable.contains(parameters)) {
      sys.error("Function does not exist: " + name + "(" + parameters + ")")
    }
    functionTable(parameters)
  }
}

class DefinitionTable {
  val definitions = mutable.HashMap[String, TypeDefinition]()
  val symbolTable = mutable.Stack[mutable.HashMap[String, SymbolGroup]]()
  push()

  def addDefinition(typeDefinition: TypeDefinition): Unit = {
    val name = typeDefinition.key().name
    if (definitions.contains(name)) {
      sys.error("Definition has already been defined: " + typeDefinition.identifier)
    }
    definitions(name) = typeDefinition
  }

  def lookupDefinition(`type`: SimpleType): TypeDefinition = {
    val name = `type`.identifier
    if (!definitions.contains(name)) {
      sys.error("Type has not been defined: " + name)
    }
    definitions(name)
  }

  def push(): Unit = symbolTable.push(mutable.HashMap[String, SymbolGroup]())

  def pop(): Unit = symbolTable.pop()

  def addSymbol(variable: VariableDeclaration): Unit = {
    val name = variable.identifier
    val currentTable = symbolTable.head
    if (currentTable.contains(name)) {
      sys.error("Variable already defined: " + name)
    }
    currentTable(name) = new VariableGroup(variable)
  }

  def addSymbol(param: Parameter): Unit = {
    val name = param.identifier
    val currentTable = symbolTable.head
    if (currentTable.contains(name)) {
      sys.error("Parameter already defined: " + name)
    }
    currentTable(name) = new ParameterGroup(param)
  }

  def addSymbol(function: FunctionDefinition): Unit = {
    val name = function.identifier
    val currentTable = symbolTable.head
    if (currentTable.contains(name)) {
      currentTable(name).asInstanceOf[FunctionGroup].addFunction(function)
    } else {
      val functionGroup = new FunctionGroup(function.returnType)
      functionGroup.addFunction(function)
      currentTable(name) = functionGroup
    }
  }

  def updateSymbol(variableDeclaration: VariableDeclaration): Unit = ???

  def updateSymbol(parameter: Parameter): Unit = ???

  def containsSymbol(key: Key): Boolean = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(_) => true
      case None => false
    }
  }

  def containsSymbolFail(key: Key): Unit = if (!containsSymbol(key)) {
    sys.error("Does not contain symbol: " + key.name)
  }

  def lookupSymbol(key: Key): Symbol = {
    symbolTable.find(_.contains(key.name)) match {
      case Some(table) => table(key.name) match {
        case functionGroup: FunctionGroup => functionGroup.lookupFunction(key.asInstanceOf[FunctionKey])
        case variableGroup: VariableGroup => variableGroup.variableDeclaration
        case parameterGroup: ParameterGroup => parameterGroup.parameter
      }
      case None => sys.error("No symbol, " + key.name + ", was declared")
    }
  }
}