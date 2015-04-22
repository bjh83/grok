package com.grok

/**
 * Created by brendan.
 */
class TypeTable(val table: Map[Type, Set[Type]]) {
  def derivesOrFail(base: Type, derived: Type): Unit = {
    if (!derives(base, derived)) {
      sys.error("Base type, " + base + ", does not derive expected type, " + derived)
    }
  }

  def derives(base: Type, derived: Type): Boolean = (base, derived) match {
    case (baseFunction: FunctionType, derivedFunction: FunctionType) => functionDerives(baseFunction, derivedFunction)
    case (_: FunctionType, _) => false
    case (_, _: FunctionType) => false
    case _ => generalDerives(base, derived)
  }

  def computeUpperBound(types: Set[Type]): Type = {
    val allTypesDerivations: Set[Set[Type]] = types.map(allDerivations)
    val upperBoundCandidates: Set[Type] = allTypesDerivations.reduce((left: Set[Type], right: Set[Type]) => left.intersect(right))
    (upperBoundCandidates - (TopType, UnitType)).toList match {
      case List(upperBound) => upperBound
      case List() => sys.error("No upper bound exists for types: " + types)
      case _ => sys.error("Multiple ambiguous upper bounds exist.")
    }
  }

  private def allDerivations(base: Type): Set[Type] = {
    val derivations = table(base) - base // Including the type itself will result in a cycle.
    derivations ++ derivations.flatMap(allDerivations)
  }

  private def generalDerives(base: Type, derived: Type): Boolean = {
    table(base).map(nextBase => derives(nextBase, derived)).reduce((left, right) => left || right)
  }

  private def functionDerives(baseType: FunctionType, derivedType: FunctionType): Boolean = {
    val FunctionType(baseParams, baseReturn) = baseType
    val FunctionType(derivedParams, derivedReturn) = derivedType
    if (baseParams.size != derivedParams.size) {
      false
    } else {
      // Derivation of parameters is reversed.
      derivedParams.zip(baseParams)
        .map { case (derived, base) => derives(derived, base) }
        .reduce((left, right) => left && right) &&
        derives(baseReturn, derivedReturn)
    }
  }
}
