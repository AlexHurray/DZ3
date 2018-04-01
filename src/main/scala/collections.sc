(1 to 10)
  .filter { x => // выбираем только четные
    x % 2 == 0
  }
  .map { x => // применяем анонимную функцию к каждому элементу
    x * 2
  }.toList


case class Person(name: String, surname: String)
val mp = Map(1 -> "Mike", 2 -> "Alex", 3 -> "Bob")
val mpSurname = mp.map { case (key, name) =>
  key -> Person(name, s"${name}inzi")
}