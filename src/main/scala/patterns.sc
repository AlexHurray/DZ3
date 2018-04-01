def matchAny(x: Any) = {
  x match {
    case "10" => println("This is 10")
    case List(x, y, _*) => println(s"First two elements are $x and $y")
    case value: String => println(s"This is String == $value")
    case _ => println("WTF")
  }
}
matchAny(List(1, 2, 3, 4, 5, 6))
matchAny("Hello")
matchAny(Vector(1, 2, 3))