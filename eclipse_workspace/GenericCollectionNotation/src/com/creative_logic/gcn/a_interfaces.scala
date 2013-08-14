package com.creative_logic.gcn

import scala.collection.immutable.ListMap

object Interfaces {
  object Kind extends Enumeration {
    type Kind = Value
    val Base =                  Value("Base") //exactly zero instances of this exist - used as root object for "collections" containing more then one of the non-abstract types 
    val UnitToBaseAssociation = Value("UnitToBaseAssociation")
    val BaseGroup =             Value("BaseGroup")
    val Unit =                  Value("Unit")
    
    def isAbstract(kindValue: Kind.Value): Boolean = kindValue.id == Base.id
  }
  
  trait BaseCharacteristics {
    val isInsertOrdered: Boolean = false //defaults to assuming insertion ordering is not significant; i.e. Java Set/Map.keySet()
    val hasDuplicates: Boolean = false   //defaults to an array not allowing duplicates; i.e. Java Set
    val hasEmptys: Boolean = false       //defaults to not allowing values with isEmpty
    val isImpureShallow: Boolean = false //defaults to assuming members are uniform at surface; same base class (excluding OBJECT)
    val isImpureDeep: Boolean = false    //defaults to assuming members are uniform from surface to every leaf; same base class (excluding OBJECT)
    val kindShallow: Kind.Value
  }
  
  trait Base extends BaseCharacteristics {
    val kind: Kind.Value                 //exactly zero instances of this exist
    val isEmpty: Boolean = false
  }

  trait UnitToBaseAssociation extends Base {
    final override val kind = Kind.UnitToBaseAssociation     //defaults to behave like an immutable Java HashMap<GcnValue, GcnObject>
    val content: ListMap[Unit, Base]
  }

  trait BaseGroup extends Base {
    final override val kind = Kind.BaseGroup  //defaults to behave like an immutable Java HashSet<GcnObject>, but implemented like immutable HashMap<Integer, GcnObject> where Integer is ranged 0..(size - 1)
    val content: Seq[Base]
    val toUnitToBaseAssociation: UnitToBaseAssociation
  }
  
  object UnitType extends Enumeration {
    type ConcreteType = Value                          //strongly influenced by Java types
    val String =  Value("String")                      //String
    val Boolean = Value("Boolean")                     //String with the possible exact (ignoring case) values of {"true", "false"}
    val Trinary = Value("Trinary")                     //String with the possible exact (ignoring case) values of {"white", "grey", "black"}
    val Identifier =  Value("Identifier")              //String starting with character from [A..Z||a..Z] and followed only by characters [A..Z||a..z||0..9||_]
    val IntegerSigned32 = Value("IntegerSigned32")     //int
    val IntegerSigned64 = Value("IntegerSigned64")     //long
    val IntegerBig = Value("IntegerBig")               //BigInteger
    val Decimal32 = Value("Decimal32")                 //float      (including "NaN")
    val Decimal64 = Value("Decimal64")                 //double     (including "NaN")
    val DecimalBig = Value("DecimalBig")               //BigDecimal (including "NaN")
    val DateTimeInMillis = Value("DateTimeinMillis")   //(IntegerSigned64, String) (based on ISO8601 standard 0 based at "1970-01-01T00:00:00.000ZZ", time zone id) - per JodaTime
    val DateTimeString = Value("DateTimeString")       //(String, String) (A string of the precise format derived from ISO8601 standard of "YYYY-MM-DDTHH:MM:SS.SSSZZZ", time zone id) - per JodaTime
    val Currency32 = Value("Currency32")               //(String, int, int) =>        (name, value, number of digits behind the decimal point) example for USD $100 is ("USD", 10000, 2)
    val Currency64 = Value("Currency64")               //(String, long, int) =>       "
    val CurrencyBig = Value("CurrencyBig")             //(String, BigInteger, int) => "
    val LongitudeLatitude = Value("LongitudeLatitude") //(long, int) => (value, number of digits behind the decimal point)
    //TODO: add other types here 
  }
  
  trait Unit extends Base {
    final override val kind = Kind.Unit  //defaults to behave like an immutable Java GcnObject, but implemented like immutable HashMap<Integer, GcnObject> where Integer is fixed at 0
    final override val kindShallow = kind
    val concreteType: UnitType.Value
    val toBaseGroup: BaseGroup
  }
}
