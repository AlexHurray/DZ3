case class Person(id: Int, name: String, age: Int, phone: String)

implicit def personToMapPersonInfo(p: Person): Map[Int, (String, Int, String)] = {
  Map(p.id -> (p.name, p.age, p.phone))
}

implicit def personListToMapPersonInfo(p: List[Person]): Map[Int, (String, Int, String)] = {
  p.flatMap(x => x).toMap
}

val p = Person(1, "Bob", 25, "+70001231212")

val mp: Map[Int, (String, Int, String)] = Person(1, "Bob", 25, "+70001231212")

val mpL: Map[Int, (String, Int, String)] = List(
  Person(1, "Bob", 25, "+70001231212"),
  Person(2, "Bill", 26, "+70001231213")
)