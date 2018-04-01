class Foo {
  var name1: String = { println("Init name1"); "Value" }
  val name2: String = { println("Init name2"); "Value" }
  lazy val name3: String = { println("Init name3"); "Value" }
}
val f = new Foo
println("after init")
f.name1 = "newValue"
//ошибка компиляции переприсвоение константе
//f.name2 = "newValue"
f.name3