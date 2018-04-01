import scala.util.Random

def funcName1(funcParam: String): String = { "<<" + funcParam + ">>"
}
def funcName2(funcParam: String) = "<<" + funcParam + ">>"
val funcName3 = (funcParam: String) => "<<" + funcParam + ">>"
//funcNameN("Hello")


def callByValue(x: Int): Unit = {
  println(s"First call x: $x")
  println(s"Second call x: $x")
}

def callByName(x: => Int) = {
  println(s"First call x: $x")
  println(s"Second call x: $x")
}

callByValue(Random.nextInt(10))
callByName(Random.nextInt(10))