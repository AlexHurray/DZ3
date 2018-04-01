class TestClass(val int: Int, var str: String, inner: Long*) {
  def this(int: Int, inner: Long*) = {
    this(int, ":", inner: _*)
  }

  def publicMethod(): Unit = {
    println(inner.mkString(str))
    println ("public")
  }

  private def privateMethod(): Unit = {
    println("private")
  }
}

val testClass = new TestClass(1, 20l, 201l)
testClass.publicMethod()