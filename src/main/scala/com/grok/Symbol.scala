package com.grok

/**
 * Created by brendan.
 */
sealed abstract class SymbolDefinition[+S, +K <: Key] {
  def symbol: S

  def key: K

  def `type`: Type

  def buildSymbolGroup(): SymbolGroup[_, _, _]
}

sealed abstract class SymbolInstance[+T, +K <: Key] {
  def symbol: T

  def key: K
}

sealed abstract class Key {
  def name: String
}

case class VariableKey(name: String) extends Key

abstract class VariableSymbolDefinition extends SymbolDefinition[VariableDeclaration, VariableKey] {

  def key = VariableKey(symbol.identifier)

  def buildSymbolGroup() = new VariableGroup(symbol)

  def `type` = symbol.varType.get
}

case class ParameterSymbolDefinition(parameter: Parameter) extends VariableSymbolDefinition {
  // TODO(Brendan): Fix this hack!!!
  def symbol = VariableDeclaration(IMMUTABLE, parameter.identifier, Some(parameter.paramType), Variable(parameter.identifier))
}

case class NormalVariableSymbolDefinition(symbol: VariableDeclaration) extends VariableSymbolDefinition

case class VariableSymbolInstance(symbol: String) extends SymbolInstance[String, VariableKey] {
  def key = VariableKey(symbol)
}


case class FunctionKey(name: String, parameters: List[Type]) extends Key {
  var resolver: Set[FunctionType] => Set[FunctionType] = FunctionKey.defaultResolver
  def toType(returnType: Type): Type = FunctionType(parameters, returnType)
}

object FunctionKey {
  def defaultResolver(candidates: Set[FunctionType]): Set[FunctionType] = candidates
}

case class FunctionSymbolDefinition(symbol: FunctionDefinition)
  extends SymbolDefinition[FunctionDefinition, FunctionKey] {

  def key = FunctionKey(symbol.identifier, symbol.parameters.map(_.paramType))

  def buildSymbolGroup() = {
    val group = new InitialFunctionGroup(symbol.identifier, symbol.returnType)
    group.add(this)
    group
  }

  def `type` = key.toType(symbol.returnType)
}

case class FunctionSymbolInstance(symbol: String, parameters: List[Type]) extends SymbolInstance[String, FunctionKey] {
  def key = FunctionKey(symbol, parameters)
}