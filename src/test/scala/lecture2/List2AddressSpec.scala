package lecture2

import org.scalatest.{FlatSpec, Matchers}

class List2AddressSpec extends FlatSpec with Matchers {
  case class Address(street: String, house: Int, apartment: Option[Int]) {
    override def toString: String = {
      street + ", " + house + apartment.fold("")(", " + _)
    }
  }

  implicit def listToAddress(xs: List[Any]): Address = {
    val exeprionMessage = "Can't construct Address from List:"

    xs match{
      case List(street: String, house: Int, apartment: Int, _*) => {
        if (house <= 0 || apartment <= 0)
          throw new Exception(exeprionMessage + xs.toString())
        Address(street, house, Some(apartment))
      }
      case List(street: String, house: Int, Some(apartment: Int), _*) => {
        if (house <= 0 || apartment <= 0)
          throw new Exception(exeprionMessage + xs.toString())
        Address(street, house, Some(apartment))
      }
      case List(street: String, house: Int, _*) => {
        if (house <= 0)
          throw new Exception(exeprionMessage + xs.toString())
        Address(street, house, None)
      }
      case _ =>
        throw new Exception(exeprionMessage + xs.toString())
    }
  }

  "List (String, Int, Int)" should "transform into Address" in {
    val list = List("Ленина", 12, 32)
    val address:Address = list
    address shouldBe Address("Ленина", 12, Some(32))
  }

  "List (String, Int, Option[Int])" should "transform into Address" in {
    val list = List("Ленина", 12, Some(32))
    val address:Address = list
    address shouldBe Address("Ленина", 12, Some(32))
  }

  "List (String, Int, Int + other vals)" should "transform into Address" in {
    val list = List("Ленина", 12, 32, "AAA")
    val address:Address = list
    address shouldBe Address("Ленина", 12, Some(32))
  }

  "List (String, Int, None)" should "transform into Address" in {
    val list = List("Ленина", 12, None)
    val address:Address = list
    address shouldBe Address("Ленина", 12, None)
  }

  "List (String, Int)" should "transform into Address" in {
    val list = List("Ленина", 12)
    val address:Address = list
    address shouldBe Address("Ленина", 12, None)
  }

  "List (String, Int + incorrect vals)" should "transform into Address" in {
    val list = List("Ленина", 12, "!@#!@#!@#")
    val address:Address = list
    address shouldBe Address("Ленина", 12, None)
  }

  "an error" should "occur if list is empty" in {
    val list = List()
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "an error" should "occur if list has only one field" in {
    val list = List("Ленина")
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "an error" should "occur if first val is not string" in {
    val list = List(10, 10, 10)
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "an error" should "occur if second val is not string" in {
    val list = List("Ленина", "12", 32)
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "an error" should "occur if second val is negative" in {
    val list = List("Ленина", -12, 32)
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "an error" should "occur if third val is negative" in {
    val list = List("Ленина", 12, -32)
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "an error" should "occur if third val is Some(negative)" in {
    val list = List("Ленина", 12, Some(-32))
    intercept[java.lang.Exception] {
      val address:Address = list
    }
  }

  "Address.toString() result" should "be without 'Some'" in {
    val address: Address = Address("Ленина", 12, Some(32))
    address.toString shouldBe "Ленина, 12, 32"
  }
}