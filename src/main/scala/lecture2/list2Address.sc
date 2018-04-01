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


def printAddress(address: Address): Unit = {
  println(address)
}


printAddress(List("Ленина", 12, Some(11)))
printAddress(List("Ленина", 12, None))
printAddress(List("Ленина", 12, 11))
printAddress(List("Ленина", 12))
printAddress(List("Ленина", 12, 11, "ASD","GGGGG"))
printAddress(List("Ленина", 12, "!@#!@#!@#"))
printAddress(List("Ленина"))