object Solution extends App with Logging {
  info("it's right message")
  error("some error occurred")
  println("back to default")

  val companion1 = Companion()
  val companion2 = Companion()
  companion1.greeting
  companion2.greeting
}
