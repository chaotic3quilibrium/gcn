package com.creative_logic.gcn

object Interfaces {
  object Kind extends Enumeration {
    type Kind = Value
    val Base =                  Value("Base") //exactly zero instances of this exist - used as root interface for "collections" containing more then one of the non-abstract types 
    val UnitToBaseAssociation = Value("UnitToBaseAssociation")
    val BaseGroup =             Value("BaseGroup")
    val Unit =                  Value("Unit")
    
    def isAbstract(kindValue: Kind.Value): Boolean = kindValue.id == Base.id
  }
  
  trait Base {                                           //exactly zero instances of this exist
    def canEqual(other: Any) = other.isInstanceOf[Base]  //should always be false unless overridden in descendant
    override def equals(other: Any): Boolean =
      other match {
        case that: Base => (
                 that.canEqual(this)
              && (kind == that.kind)  
              && (content == that.content)
            )
        case _ => false
    }
    override def hashCode = 41 * (41 * (41 + kind.hashCode)) + content.hashCode
    val kind: Kind.Value                                 //constructor parameter: must be of Intefaces.Kind where !Kind.isAbstract for kind
    def content: Any                                     //constructor parameter: 
    def isInsertOrdered: Boolean                         //constructor parameter: false if content ordering not significant
    def kindShallow: Kind.Value                          //content evaluation: base type of the values present in the content
    def isEmpty: Boolean                                 //content evaluation: false if any content present
    def hasEmptys: Boolean                               //content evaluation: false if all contained Base.isEmpty is false
    def hasDuplicates: Boolean                           //content evaluation: false if all contained Base is not equal to any other contained Base
    def isImpureShallow: Boolean                         //content evaluation: false if all contained Base.kind are equal
    def isImpureDeep: Boolean                            //content evaluation: false if all contained Base.content's Base are equal
  }
  
  trait UnitToBaseAssociation extends Base {
    final override def canEqual(other: Any) = other.isInstanceOf[UnitToBaseAssociation]
    final override val kind = Kind.UnitToBaseAssociation //defaults to behave like an immutable Java HashMap<Unit, Base>
    override def content: Map[Unit, Base]                //if isInsertOrdered is true, instance will be of ListMap
  }

  trait BaseGroup extends Base {                         //defaults to behave like an immutable Java HashSet<Base>, but implemented like immutable HashMap<Unit.Integer32, Base> where Integer is ranged 0..(size - 1)
    final override def canEqual(other: Any) = other.isInstanceOf[BaseGroup]
    final override val kind = Kind.BaseGroup
    override def content: Seq[Base]
    final def canBeUnitToBaseAssociationKeySet: Boolean = (
           (kindShallow == Kind.Unit)
        && (!isEmpty)
        && (!hasEmptys)
        && (!hasDuplicates)
      )
    def toUnitToBaseAssociation: UnitToBaseAssociation   //convenience conversion to UnitToBaseAssociation
  }
  
  object UnitType extends Enumeration {
    type ConcreteType = Value                            //strongly influenced by Java types
    val String =  Value("String")                        //String
    val Boolean = Value("Boolean")                       //String with the possible exact (ignoring case) values of {"true", "false"}
    val Trinary = Value("Trinary")                       //String with the possible exact (ignoring case) values of {"white", "grey", "black"}
    val Identifier =  Value("Identifier")                //String starting with character from [A..Z||a..Z] and followed only by characters [A..Z||a..z||0..9||_]
    val IntegerSigned32 = Value("IntegerSigned32")       //int
    val IntegerSigned64 = Value("IntegerSigned64")       //long
    val IntegerBig = Value("IntegerBig")                 //BigInteger
    val Decimal32 = Value("Decimal32")                   //float      (including "NaN")
    val Decimal64 = Value("Decimal64")                   //double     (including "NaN")
    val DecimalBig = Value("DecimalBig")                 //BigDecimal (including "NaN")
    val DateTimeInMillis = Value("DateTimeinMillis")     //(IntegerSigned64, String) (based on ISO8601 standard 0 based at "1970-01-01T00:00:00.000ZZ", time zone id) - per JodaTime
    val DateTimeString = Value("DateTimeString")         //(String, String) (A string of the precise format derived from ISO8601 standard of "YYYY-MM-DDTHH:MM:SS.SSSZZZ", time zone id) - per JodaTime
    val Currency32 = Value("Currency32")                 //(String, int, int) =>        (name, value, number of digits behind the decimal point) example for USD $100 is ("USD", 10000, 2)
    val Currency64 = Value("Currency64")                 //(String, long, int) =>       "
    val CurrencyBig = Value("CurrencyBig")               //(String, BigInteger, int) => "
    val LongitudeLatitude = Value("LongitudeLatitude")   //(long, int) => (value, number of digits behind the decimal point)
    //TODO: add other types here 
  }
  
  trait Unit extends Base {                              //defaults to behave like an immutable Java Base, but implemented like immutable HashMap<Unit.IntegerSigned32, Base> where IntegerSigned32 is fixed at 0
    final override def canEqual(other: Any) = other.isInstanceOf[Unit]
    final override val kind = Kind.Unit
    override def content: String
    final override val isInsertOrdered = false
    final override val kindShallow = kind
    //final override val isEmpty = false
    final override val hasEmptys = false
    final override val hasDuplicates = false
    final override val isImpureShallow = false
    final override val isImpureDeep = false
    val concreteType: UnitType.Value
    def toBaseGroup: BaseGroup                           //convenience conversion to BaseGroup
  }
}
