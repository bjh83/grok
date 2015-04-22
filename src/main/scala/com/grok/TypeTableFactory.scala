package com.grok

import TypeTableFactory._

/**
 * Created by brendan.
 */
class TypeTableFactory(val unions: Set[UnionDefinition],
                       val structs: Set[StructDefinition]) {

  // TopType and BottomType are not included since UnitType may not derive TopType otherwise a derivation cycle would
  // exist.
  val builtIn = Map[Type, Set[Type]](
    BoolType -> Set(BoolType, UnitType, TopType),
    IntegralType -> Set(IntegralType, FloatingPointType, UnitType, TopType),
    FloatingPointType -> Set(FloatingPointType, UnitType, TopType),
    UnitType -> Set(UnitType)
  )

  def build(): TypeTable = new TypeTable(buildMap())

  private def buildMap(): Map[Type, Set[Type]] = {
    val trivialMap = addTrivialDerivations(allTypes().map(elem => (elem, Set[Type]())).toMap)
    val withBuiltIn = mergeMaps(trivialMap, builtIn)
    val unionsMap = unions.map(union => (unionToType(union), union)).toMap
    val withUnions = mergeMaps(withBuiltIn, buildUnionTypeMap(unionsMap))
    withUnions
  }

  private def allTypes(): Set[Type] = {
    unions.map(unionToType) ++ structs.map(structToType) ++ builtIn.keys
  }
}

object TypeTableFactory {
  def unionToType(union: UnionDefinition): SimpleType = {
    val name = union.identifier
    val parameters = union.typeParameters
    SimpleType(name, parameters)
  }

  def structToType(struct: StructDefinition): SimpleType = {
    val name = struct.identifier
    val parameters = struct.typeParameters
    SimpleType(name, parameters)
  }

  def addTrivialDerivations(types: Map[Type, Set[Type]]): Map[Type, Set[Type]] = {
    types.map { case (key, derivations) => (key, derivations ++ Set(key, UnitType)) }
  }

  def buildUnionTypeMap(unions: Map[SimpleType, UnionDefinition]): Map[Type, Set[Type]] = {
    val unionsToMembers: Set[(SimpleType, Set[Type])] = unions.mapValues(_.members.toSet).toSet
    val memberUnionPairs = unionsToMembers.flatMap[(Type, SimpleType), Set[(Type, SimpleType)]] { case (union: SimpleType, members: Set[Type]) =>
        members.map { member: Type => (member, union) }
    }
    memberUnionPairs.groupBy { case (member, union) => member } // Map pairs to members
      .mapValues(_.map { case (member, union) => union } ) // reduce pair to just union
  }

  def addTopTypeAndBottomType(types: Map[Type, Set[Type]]): Map[Type, Set[Type]] = {
    val allTypes = types.keySet
    (types + (BottomType -> allTypes)).mapValues(_ + TopType) + (TopType -> Set(TopType))
  }

  def mergeMaps[K, V](left: Map[K, Set[V]], right: Map[K, Set[V]]): Map[K, Set[V]] = {
    val allValues = left.toSeq ++ right.toSeq
    allValues.groupBy { case (key, values) => key }.mapValues(_.map { case (_, values) => values }.toSet.flatten)
  }
}