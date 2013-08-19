package com.creative_logic.gcn

import scala.util.matching.Regex

//import com.github.nscala_time.time.Imports._ //from https://github.com/nscala-time/nscala-time
//import org.joda.time.DateTime                //2.3 from http://sourceforge.net/projects/joda-time/files/joda-time/2.3/

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
    override def toString = "Base(" + kind + "," + content + ")"
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
    val Trinary = Value("Trinary")                       //String with the possible exact (ignoring case) values of {"white", "gray", "black"}
    val IdentifierUsa =  Value("Identifier")             //String must match regular expression "[A-Za-z_][A-Za-z_\d]*"
    val IdentifierInternational =  Value("IdentifierInternational") //String must match regular expression "[\p{L}|_][\p{L}|_|\d]*"
    val IntegerSigned32 = Value("IntegerSigned32")       //int
    val IntegerSigned64 = Value("IntegerSigned64")       //long
    val IntegerSignedNBit = Value("IntegerSignedNBit")   //(numberOfBits, String) String must match regular expression "0|-?[1-9]\d*"
    val Decimal32 = Value("Decimal32")                   //float      (including "NaN")
    val Decimal64 = Value("Decimal64")                   //double     (including "NaN")
    val DecimalNBit = Value("DecimalNBit")               //(BigDecimal, int) (value, numberOfBits) (including "NaN")
    val DecimalExact32 = Value("DecimalExact32")         //(int, int)  =>              (value, number of digits behind the decimal point) example for 100.01 is (10001, 2)
    val DecimalExact64 = Value("DecimalExact64")         //(long, int) =>              "
    val DecimalExactNBit = Value("DecimalExactNBit")     //(IntegerSignedNBit, int) => "
    val DateTimeInMillis = Value("DateTimeinMillis")     //(IntegerSigned64, String) (based on ISO8601 standard 0 based at "1970-01-01T00:00:00.000ZZ", time zone id) - per JodaTime - http://www.joda.org/joda-time/
    val DateTimeString = Value("DateTimeString")         //(String, String) (A string of the precise format derived from ISO8601 standard of "YYYY-MM-DDTHH:MM:SS.SSSZZZ", time zone id) - per JodaTime - http://www.joda.org/joda-time/
    val Currency32 = Value("Currency32")                 //(DecimalExact32, String) =>   (value, number of digits behind the decimal point) example for USD $100 is ("USD", 10000, 2) - currency names per ISO 4217 - http://www.xe.com/iso4217.php
    val Currency64 = Value("Currency64")                 //(DecimalExact64, String) =>   "
    val CurrencyNBit = Value("CurrencyNBit")             //(DecimalExactNBit, String) => "
    val LongitudeLatitude = Value("LongitudeLatitude")   //((long, int), (long, int)) => (value, number of digits behind the decimal point)
    val Uuid = Value("UUID")                             //OSF's UUID - http://en.wikipedia.org/wiki/Universally_unique_identifier
    val Uri = Value("URI")                               //Parent of URL/URN - see http://en.wikipedia.org/wiki/Uniform_resource_identifier (also works for files as in "file://host/path" - see http://en.wikipedia.org/wiki/File_URI_scheme)
    //TODO: add other types here 
  }

  private val regExIntegerPositive = """0|[1-9]\d*"""
  private val regExIntegerSigned = """0|-?[1-9]\d*"""
  
  val unitTypeRegExes: Map[UnitType.Value, List[Regex]] = Map(
        //UnitType.String -> List("*".r)
        UnitType.Boolean -> List("""false|true|no|yes|0|1""".r)
      , UnitType.Trinary -> List("""black|gray|white|grey""".r)
      , UnitType.IdentifierUsa -> List("""[A-Za-z_][A-Za-z_\d]*""".r)
      , UnitType.IdentifierInternational -> List("""[\p{L}|_][\p{L}|_|\d]*""".r)
      , UnitType.IntegerSigned32 -> List(regExIntegerSigned.r)
      , UnitType.IntegerSigned64 -> List(regExIntegerSigned.r)
      , UnitType.IntegerSignedNBit -> List(regExIntegerSigned.r, regExIntegerPositive.r)
      //TODO: must extend this to cover all the types
    )
  
  object Trinary extends Enumeration {
    type Kind = Value
    val White = Value("white") 
    val Grey =  Value("gray")
    val Black = Value("black")
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
