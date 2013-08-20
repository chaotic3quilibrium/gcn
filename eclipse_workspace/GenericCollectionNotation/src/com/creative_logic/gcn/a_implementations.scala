package com.creative_logic.gcn

import com.creative_logic.gcn.Interfaces._

object Implementations {
  final object UnitToBaseAssociation {
    //Due to Map.keySet being type invariant, removed createWithMap(keyValues: Map[Unit, Base]) and createWithListMap(keyValues: ListMap[Unit, Base])
    def create(keys: BaseGroup = BaseGroup.create(), values: BaseGroup = BaseGroup.create(), keySetIsInsertOrdered: Boolean = false): UnitToBaseAssociation =
      if (!keys.isEmpty) new UnitToBaseAssociationImpl(keys, values, keySetIsInsertOrdered)
      else UnitToBaseAssociationEmpty
  }
  private final object UnitToBaseAssociationEmpty extends UnitToBaseAssociation {
    final override def toString = "UnitToBaseAssociation(Empty)"
    final override val content = (BaseGroup.create(), BaseGroup.create())
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
    final override def toString = "UnitToBaseAssociation(" + isInsertOrdered + "," + kindShallow + "," + content + ")"
    final override val content = (keys, values)
    final override val isInsertOrdered = keySetIsInsertOrdered
    final override val kindShallow = values.kindShallow
    final override val isEmpty = values.isEmpty               //should always be false given values.content.length > 0
    final override val hasEmptys = values.hasEmptys
    final override val hasDuplicates = values.hasDuplicates 
    final override val isImpureShallow = values.isImpureShallow 
    final override val isImpureDeep = values.isImpureDeep 
  }
  
  final object BaseGroup {
    //Due to Set being type invariant, removed createWithSet(values: Set[Base]) and createWithListSet(values: ListSet[Base])
    def create(values: Seq[Base] = Seq(), valuesIsInsertOrdered: Boolean = false): BaseGroup =
      if (!values.isEmpty) new BaseGroupImpl(values, valuesIsInsertOrdered)
      else BaseGroupEmpty
  }
  private final object BaseGroupEmpty extends BaseGroup {
    final override def toString = "BaseGroup(Empty)"
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
    final override def toString = "BaseGroup(" + isInsertOrdered + "," + kindShallow + "," + content + ")"
    final override val content = values;
    final override val isInsertOrdered = valuesIsInsertOrdered
    final override val isEmpty = false
    final override val hasEmptys = content.exists(_.isEmpty)
    final override val hasDuplicates = content.length > content.distinct.length
    final override val isImpureShallow = {
      val kind = content.head.kind
      content.exists(_.kind != kind)
    }
    final override val kindShallow = if (isImpureShallow) Kind.Base else values.head.kind
    final override val isImpureDeep = isImpureShallow || content.exists(_.isImpureDeep)
    final override lazy val toUnitToBaseAssociation: UnitToBaseAssociation = {
      val index = BaseGroup.create((0 until content.length).map(new UnitIntegerSigned32(_)))
      UnitToBaseAssociation.create(index, this)
    }
  }

  abstract class UnitImpl(rawValue: String) extends Unit {
    final override def toString = "Unit(" + concreteType + "," + content + ")"
    final override val content = rawValue
    final override val isEmpty = rawValue.length() == 0
    final override lazy val toBaseGroup: BaseGroup = BaseGroup.create(Seq(this))
  }
  final class UnitString(rawValue: String) extends UnitImpl(rawValue) {
    final override val concreteType = UnitType.String
  }
  final class UnitBoolean(val rawValue: Boolean) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.Boolean
  }
  final class UnitTrinary(val rawValue: Trinary.Value) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.Boolean
  }
  final class UnitIdentifier(val rawValue: String) extends UnitImpl(rawValue) {
    require(rawValue.matches("""[A-Za-z_][A-Za-z_0-9]*"""), "rawValue [" + rawValue+ "] must start with an instance of {letter, underscore} and then be followed by 0 to n instances of {letter, number, underscore}")
    final override val concreteType = UnitType.IdentifierUsa
  }
  final class UnitIntegerSigned32(val rawValue: Int) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.IntegerSigned32
  }
  final class UnitIntegerSigned64(val rawValue: Long) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.IntegerSigned64
  }
  final class UnitIntegerNBit(val rawValue: BigInt) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.IntegerSignedNBit
  }
  final class UnitDecimal32(val rawValue: Float) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.Decimal32
  }
  final class UnitDecimal64(val rawValue: Double) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.Decimal64
  }
  final class UnitDecimalNBit(val rawValue: BigDecimal) extends UnitImpl(rawValue.toString) {
    final override val concreteType = UnitType.DecimalNBit
  }
  //TODO: fill in the remaining UnitTypes implementations; DateTimeInMillis, DateTimeString, Currency32, etc. 
}
