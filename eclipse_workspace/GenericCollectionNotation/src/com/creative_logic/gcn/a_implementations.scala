package com.creative_logic.gcn

import com.creative_logic.gcn.Interfaces._
import scala.collection.immutable.ListMap
import scala.collection.immutable.ListSet

object Implementations {
  final object UnitToBaseAssociation {
    def createWithMap(keyValues: Map[Unit, Base]): UnitToBaseAssociation = 
      if (!keyValues.isEmpty) {
        val (keys, values) = keyValues.unzip
        createWithParallelBaseGroups(BaseGroup.createWithSeq(keys.toSeq), BaseGroup.createWithSeq(values.toSeq))
      }
      else UnitToBaseAssociationEmpty
    def createWithListMap(keyValues: ListMap[Unit, Base]): UnitToBaseAssociation = 
      if (!keyValues.isEmpty) {
        val (keys, values) = keyValues.unzip
        createWithParallelBaseGroups(BaseGroup.createWithSeq(keys.toSeq), BaseGroup.createWithSeq(values.toSeq), true)
      }
      else UnitToBaseAssociationEmpty
    def createWithParallelBaseGroups(keys: BaseGroup, values: BaseGroup, keySetIsInsertOrdered: Boolean = false): UnitToBaseAssociation =
      if (!keys.isEmpty) new UnitToBaseAssociationImpl(keys, values, keySetIsInsertOrdered)
      else UnitToBaseAssociationEmpty
  }
  private final object UnitToBaseAssociationEmpty extends UnitToBaseAssociation {
    final override val content = ListMap.empty[Unit, Base]
    final override val isInsertOrdered = false
    final override val kindShallow = Kind.Base
    final override val isEmpty = true
    final override val hasEmptys = false
    final override val hasDuplicates = false
    final override val isImpureShallow = false
    final override val isImpureDeep = false
  }
  private final class UnitToBaseAssociationImpl(keys: BaseGroup, values: BaseGroup, keySetIsInsertOrdered: Boolean = false) extends UnitToBaseAssociation {
    require(keys.canBeUnitToBaseAssociationKeySet, "keys.canBeUnitToBaseAssociationKeySet must be true")
    require(!values.isEmpty, "values must not be empty")
    require(keys.content.length == values.content.length, "keys.content.length [" + keys.content.length + "] must be equal to values.content.length [" + values.content.length + "]")
    final override val content = keys.content.map(_.asInstanceOf[Unit]).zip(values.content).toMap
    final override val isInsertOrdered = keySetIsInsertOrdered
    final override val kindShallow = values.kindShallow
    final override val isEmpty = values.isEmpty               //should always be false given values.content.length > 0
    final override val hasEmptys = values.hasEmptys
    final override val hasDuplicates = values.hasDuplicates 
    final override val isImpureShallow = values.isImpureShallow 
    final override val isImpureDeep = values.isImpureDeep 
  }
  
  final object BaseGroup {
    def createWithSet(values: Set[Base]): BaseGroup =
      if (!values.isEmpty) createWithSeq(values.toSeq)
      else BaseGroupEmpty
    def createWithListSet(values: ListSet[Base]): BaseGroup =
      if (!values.isEmpty) createWithSeq(values.toSeq, true)
      else BaseGroupEmpty
    def createWithSeq(values: Seq[Base], valuesIsOrdered: Boolean = false): BaseGroup =
      if (!values.isEmpty) new BaseGroupImpl(values, valuesIsOrdered)
      else BaseGroupEmpty
  }
  private final object BaseGroupEmpty extends BaseGroup {
    final override val content = Seq.empty[Base]
    final override val isInsertOrdered = false
    final override val kindShallow = Kind.Base
    final override val isEmpty = true
    final override val hasEmptys = false
    final override val hasDuplicates = false
    final override val isImpureShallow = false
    final override val isImpureDeep = false
    final override lazy val toUnitToBaseAssociation: UnitToBaseAssociation = UnitToBaseAssociationEmpty
  }
  private final class BaseGroupImpl(values: Seq[Base], valuesIsInsertOrdered: Boolean) extends BaseGroup {
    require(!values.isEmpty, "values must not be empty")
    final override val content = values;
    final override val isInsertOrdered = valuesIsInsertOrdered
    final override val kindShallow = if (isImpureShallow) Kind.Base else values.head.kind
    final override val isEmpty = false
    final override val hasEmptys = values.exists(_.isEmpty)
    final override val hasDuplicates = values.length > values.distinct.length
    final override val isImpureShallow = {
      val kind = values.head.kind
      content.exists(_.kind != kind)
    }
    final override val isImpureDeep = content.exists(_.isImpureDeep)
    final override lazy val toUnitToBaseAssociation: UnitToBaseAssociation = {
      val x = content.zipWithIndex.map(x => (new UnitString(x._2.toString), x._1))
      UnitToBaseAssociation.createWithListMap(ListMap[Unit, Base]() ++ x)
    }
  }

  abstract class UnitImpl(rawValue: String) extends Unit {
    final override val content = rawValue
    final override val isEmpty = rawValue.length() == 0
    final override lazy val toBaseGroup: BaseGroup = BaseGroup.createWithSeq(List(this))
  }
  final class UnitString(rawValue: String) extends UnitImpl(rawValue) {
    final override val concreteType = UnitType.String
  }
  private class UnitIntegerSigned32(val rawValue: Int) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.IntegerSigned32
  }
  //TODO: fill in the remaining UnitTypes implementations; Boolean, Trinary, Identifier, etc. 
}
