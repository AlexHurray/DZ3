def printString(implicit str: String): Unit ={
  println(str)
}

implicit val abracadabra = "WOW"

printString