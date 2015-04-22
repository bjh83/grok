package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
abstract class SymbolGroup[S, K <: Key, D <: SymbolDefinition[S, K]] {
  final val add: PartialFunction[SymbolDefinition, Unit] = {
    case definition: D => internalAdd(definition)
    case _ => sys.error("Incorrect symbol type.")
  }

  protected def internalAdd(symbol: D): Unit

  final val get: PartialFunction[Key, SymbolDefinition] = {
    case key: K => internalGet(key)
    case _ => sys.error("Incorrect symbol type.")
  }

  protected def internalGet(key: K): SymbolDefinition

  def buildWithTable(typeTable: TypeTable): SymbolGroup

  final val update: PartialFunction[SymbolDefinition, Unit] = {
    case definition: D => internalUpdate(definition)
    case _ => sys.error("Incorrect symbol type.")
  }

  protected def internalUpdate(definition: D): Unit
}

class VariableGroup(private var variableDeclaration: VariableDeclaration)
  extends SymbolGroup[VariableDeclaration, VariableKey, VariableSymbolDefinition] {

  override protected def internalAdd(symbol: VariableSymbolDefinition): Unit = {
    sys.error("Symbol, " + variableDeclaration.identifier + ", already defined in current scope.")
  }

  override protected def internalGet(key: VariableKey) = NormalVariableSymbolDefinition(variableDeclaration)

  override def buildWithTable(typeTable: TypeTable) = this

  override def internalUpdate(definition: VariableSymbolDefinition): Unit = {
    variableDeclaration = definition.symbol
  }
}

class InitialFunctionGroup(protected val name: String, protected val returnType: Type)
  extends SymbolGroup[FunctionDefinition, FunctionKey, FunctionSymbolDefinition] {

  protected val map = mutable.Map[FunctionKey, FunctionSymbolDefinition]()

  override protected def internalAdd(symbol: FunctionSymbolDefinition): Unit = {
    if (symbol.symbol.returnType == returnType) {
      if (!map.contains(symbol.key)) {
        map(symbol.key) = symbol
      } else {
        sys.error("Function with signature, " + symbol.key + ", already declared.")
      }
    } else {
      sys.error("Multiple functions of name, " + name + ", with different return types.")
    }
  }

  override protected def internalGet(key: FunctionKey): FunctionSymbolDefinition = {
    sys.error("TypeTable must be provided before function keys may be resolved.")
  }

  override protected def internalUpdate(symbol: FunctionSymbolDefinition): Unit = {
    if (symbol.symbol.returnType == returnType) {
      if (map.contains(symbol.key)) {
        map(symbol.key) = symbol
      } else {
        sys.error("Function with signature, " + symbol.key + ", was not declared.")
      }
    } else {
      sys.error("Multiple functions of name, " + name + ", with different return types.")
    }
  }

  override def buildWithTable(typeTable: TypeTable) = new FinalFunctionGroup(name, returnType, typeTable)
}

class FinalFunctionGroup(name: String,
                         returnType: Type,
                         private val typeTable: TypeTable) extends InitialFunctionGroup(name, returnType) {
  override protected def internalGet(outsideKey: FunctionKey): FunctionSymbolDefinition = {
    val keyDefinitionPairs = map.keySet
    val candidates = keyDefinitionPairs.filter { key =>
      typeTable.derives(outsideKey.toType(returnType), key.toType(returnType))
    }.map(key => key.toType(returnType).asInstanceOf[FunctionType]).toSet
    outsideKey.resolver(candidates).toList match {
      case List() => sys.error("No function named \"" + name + "\" that accepts args: " + outsideKey.parameters)
      case List(functionType) => map(FunctionKey(name, functionType.parameters))
      case _ => sys.error("Cannot resolve function named \"" + name + "\" due to ambiguous types.")
    }
  }

  override def buildWithTable(typeTable: TypeTable) = this
}