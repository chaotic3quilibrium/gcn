package com.creative_logic.gcn

import com.creative_logic.gcn.Interfaces._
import scala.collection.immutable.ListMap
import scala.collection.immutable.ListSet

object Implementations {
  final object UnitToBaseAssociation {
    def createWithMap(keyValues: Map[Unit, Base], valuesHasEmptys: Boolean = false): UnitToBaseAssociation = 
      if (!keyValues.isEmpty) new UnitToBaseAssociationImpl(ListMap[Unit, Base]() ++ keyValues, false, valuesHasEmptys)
      else UnitToBaseAssociationEmpty
    def createWithListMap(keyValues: ListMap[Unit, Base], valuesHasEmptys: Boolean = false): UnitToBaseAssociation = 
      if (!keyValues.isEmpty) new UnitToBaseAssociationImpl(keyValues, true, valuesHasEmptys)
      else UnitToBaseAssociationEmpty
  }
  private final object UnitToBaseAssociationEmpty extends UnitToBaseAssociation {
    final override val kindShallow = kind
    final override val isEmpty = true
    final override val content = ListMap.empty[Unit, Base]
  }
  private final class UnitToBaseAssociationImpl(keyValues: ListMap[Unit, Base], keySetIsInsertOrdered: Boolean, valuesHasEmptys: Boolean) extends UnitToBaseAssociation {
    require(!keyValues.isEmpty, "keyValues must not be empty")
    require(!keyValues.keySet.exists(_.isEmpty), "keyValues.keySet must not contain any instances of GcnValue.isEmpty")
    require(valuesHasEmptys || !keyValues.values.exists(_.isEmpty), "when valuesHasNones is false, keyValues.values must not contain any instances where GcnObject.isEmpty is true")
    final override val isInsertOrdered = keySetIsInsertOrdered
    final override val hasEmptys = valuesHasEmptys
    final override val isImpureShallow = {
      val kind = keyValues.head._2.kind
      content.exists(_._2.kind != kind)
    }
    final override val isImpureDeep = content.exists(_._2.isImpureDeep)
    final override val kindShallow = if (isImpureShallow) Kind.Base else keyValues.head._2.kind
    final override val content = keyValues;
  }
  
  final object BaseGroup {
    def createWithSet(values: Set[Base], valuesHasEmptys: Boolean = false): BaseGroup =
      if (!values.isEmpty) new BaseGroupImpl(values.toSeq, false, false, valuesHasEmptys)
      else BaseGroupEmpty
    def createWithSortedSet(values: ListSet[Base], valuesHasEmptys: Boolean = false): BaseGroup =
      if (!values.isEmpty) new BaseGroupImpl(values.toSeq, true, false, valuesHasEmptys)
      else BaseGroupEmpty
    def createWithSeq(values: Seq[Base], valuesIsOrdered: Boolean = false, valuesHasDuplicates: Boolean = false, valuesHasEmptys: Boolean = false): BaseGroup =
      if (!values.isEmpty) new BaseGroupImpl(values, valuesIsOrdered, valuesHasDuplicates, valuesHasEmptys)
      else BaseGroupEmpty
  }
  private final object BaseGroupEmpty extends BaseGroup {
    final override val kindShallow = kind
    final override val isEmpty = true
    final override val content = Seq.empty[Base]
    final override lazy val toUnitToBaseAssociation: UnitToBaseAssociation = UnitToBaseAssociationEmpty
  }
  private final class BaseGroupImpl(values: Seq[Base], valuesIsInsertOrdered: Boolean, valuesHasDuplicates: Boolean, valuesHasEmptys: Boolean) extends BaseGroup {
    require(!values.isEmpty, "values must not be empty")
    require(valuesHasDuplicates || (values == values.distinct), " if (valuesHasDuplicates == false), values must be distinct")
    final override val isInsertOrdered = valuesIsInsertOrdered
    final override val hasDuplicates = valuesHasDuplicates
    final override val hasEmptys = valuesHasEmptys
    final override val isImpureShallow = {
      val kind = values.head.kind
      content.exists(_.kind != kind)
    }
    final override val isImpureDeep = content.exists(_.isImpureDeep)
    final override val kindShallow = if (isImpureShallow) Kind.Base else values.head.kind
    final override val content = values;
    final override lazy val toUnitToBaseAssociation: UnitToBaseAssociation = {
      val x = content.zipWithIndex.map(x => (UnitString(x._2.toString), x._1))
      UnitToBaseAssociation.createWithListMap(ListMap[Unit, Base]() ++ x, false)
    }
  }

  abstract class UnitImpl(val content: String) extends Unit {
    final override lazy val toBaseGroup: BaseGroup = BaseGroup.createWithSeq(List(this), true, false, false)
  }
  final case class UnitString(rawValue: String) extends UnitImpl(rawValue) {
    final override val isEmpty = rawValue.length() > 0
    final override val concreteType = UnitType.String
  }
  private case class UnitIntegerSigned32(val rawValue: Int) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.IntegerSigned32
  }
  //TODO: fill in the remaining UnitTypes implementations; Boolean, Trinary, Identifier, etc. 
}
