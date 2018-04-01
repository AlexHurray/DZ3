val xs: List[Option[String]] = List(Some("String"), None, Some("Another"))

xs.flatMap(op => op.map(_.toUpperCase))

case class Person(name: String, surname: String, patronymic: Option[String] = None)

val persons = List(Person("Alex", "Green"),
  Person("Ivan", "Ivanov", Some("Ivanovich")), Person("Bill", "Murray"))

persons.foreach { case(Person(name, surname, pat)) =>
  println(s"FIO: $surname, $name${pat.fold("")(", " + _)}")
}
//output
//FIO: Green, Alex
//FIO: Ivanov, Ivan, Ivanovich
//FIO: Murray, Bill