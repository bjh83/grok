package com.grok

import scala.collection.mutable

/**
 * Created by brendan.
 */
abstract class SymbolGroup[+S, +K <: Key, +D <: SymbolDefinition[S, K]] {
  def add(symbolDef: SymbolDefinition[_, _]): Unit

  def get(key: Key): SymbolDefinition[S, K]

  def buildWithTable(typeTable: TypeTable): SymbolGroup[S, K, SymbolDefinition[S, K]]

  final val update: PartialFunction[SymbolDefinition[_, Key], Unit] = {
    case definition: D => internalUpdate(definition)
    case _ => sys.error("Incorrect symbol type.")
  }

  protected def internalUpdate(definition: SymbolDefinition[_, Key]): Unit
}

class VariableGroup(var variableDeclaration: VariableDeclaration)
  extends SymbolGroup[VariableDeclaration, VariableKey, VariableSymbolDefinition] {

  override def add(symbol: SymbolDefinition[_, _]): Unit = {
    sys.error("Symbol, " + variableDeclaration.identifier + ", already defined in current scope.")
  }

  override def get(key: Key) = NormalVariableSymbolDefinition(variableDeclaration)

  override def buildWithTable(typeTable: TypeTable) = this

  override def internalUpdate(definition: SymbolDefinition[_, Key]): Unit = {
    definition match {
      case variableDef: VariableSymbolDefinition => variableDeclaration = variableDef.symbol
      case _ => sys.error("Incorrect symbol type.")
    }
  }
}

class InitialFunctionGroup(protected val name: String, protected val returnType: Type)
  extends SymbolGroup[FunctionDefinition, FunctionKey, FunctionSymbolDefinition] {

  var map = mutable.Map[FunctionKey, FunctionSymbolDefinition]()

  override def add(definition: SymbolDefinition[_, _]): Unit = {
    definition match {
      case symbol: FunctionSymbolDefinition =>
        if (symbol.symbol.returnType == returnType) {
          if (!map.contains(symbol.key)) {
            map(symbol.key) = symbol
          } else {
            sys.error("Function with signature, " + symbol.key + ", already declared.")
          }
        } else {
          sys.error("Multiple functions of name, " + name + ", with different return types.")
        }
      case _ => sys.error("Incorrect symbol type.")
    }
  }

  override def get(key: Key): FunctionSymbolDefinition = {
    sys.error("TypeTable must be provided before function keys may be resolved.")
  }

  override def internalUpdate(definition: SymbolDefinition[_, Key]): Unit = definition match {
    case symbol: FunctionSymbolDefinition =>
      if (symbol.symbol.returnType == returnType) {
        if (map.contains(symbol.key)) {
          map(symbol.key) = symbol
        } else {
          sys.error("Function with signature, " + symbol.key + ", was not declared.")
        }
      } else {
        sys.error("Multiple functions of name, " + name + ", with different return types.")
      }
    case _ => sys.error("Incorrect symbol type.")
  }

  override def buildWithTable(typeTable: TypeTable) = {
    val functionGroup = new FinalFunctionGroup(name, returnType, typeTable)
    functionGroup.map = map
    functionGroup
  }
}

class FinalFunctionGroup(name: String,
                         returnType: Type,
                         private val typeTable: TypeTable) extends InitialFunctionGroup(name, returnType) {
  override def get(key: Key): FunctionSymbolDefinition = {
    key match {
      case actualKey: FunctionKey => internalGet(actualKey)
      case _ => sys.error("Incorrect symbol type.")
    }
  }
  protected def internalGet(outsideKey: FunctionKey): FunctionSymbolDefinition = {
    val keyDefinitionPairs = map.keySet
    val candidates = keyDefinitionPairs.filter { key =>
      typeTable.derives(outsideKey.toType(returnType), key.toType(returnType))
    }.map(key => key.toType(returnType).asInstanceOf[FunctionType]).toSet
    outsideKey.resolver(candidates).toList match {
      case List() => sys.error("No function named \"" + name + "\" that accepts args: "
        + outsideKey.parameters + ". Functions in group: " + map)
      case List(functionType) => map(FunctionKey(name, functionType.parameters))
      case _ => sys.error("Cannot resolve function named \"" + name + "\" due to ambiguous types.")
    }
  }

  override def buildWithTable(typeTable: TypeTable) = this
}