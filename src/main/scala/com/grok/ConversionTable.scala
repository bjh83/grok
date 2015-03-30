package com.grok

import scala.collection.mutable
import scala.collection.mutable.Set

/**
 * Created by brendan on 3/29/15.
 */
class ConversionTable(definitionTable: DefinitionTable) {
  val conversionTable = mutable.HashMap[Type, mutable.Set[Type]](
    BoolType -> Set(BoolType),
    IntegralType -> Set(IntegralType, FloatingPointType),
    FloatingPointType -> Set(FloatingPointType)
  )

  {
    val definitions = definitionTable.definitions.values
    val definedTypes = definitions.map(_.definesType())
    conversionTable ++= definedTypes.zip(definedTypes.map(Set(_)))
    val unions = definitions.filter(_.isInstanceOf[UnionDefinition]).map(_.asInstanceOf[UnionDefinition])
    unions.map { union =>
      val unionType = union.definesType()
      union.members.map((_, unionType))
    }.flatten.foreach { case (typ, unionType) => conversionTable(typ).add(unionType) }
  }

  def commonDerivedType(types: List[Type]): Type = {
    val conversions = types.map(conversionTable)
    val conversion = conversions.tail.fold(conversions.head)((left, right) => left.intersect(right))
    if (conversion.size > 1) {
      sys.error("Type conversion is ambiguous for: " + types)
    } else if (conversion.size == 1) {
      conversion.head
    } else {
      UnitType // Unit type is also the top type.
    }
  }

  def coerceType(actual: Type, desired: Type): Boolean = {
    conversionTable(actual).intersect(conversionTable(desired)).size == 1 || desired == UnitType
  }

  def coerceOrFail(actual: Type, desired: Type): Unit = {
    if (!coerceType(actual, desired)) {
      sys.error("Actual type, " + actual + ", cannot be viewed as " + desired)
    }
  }
}