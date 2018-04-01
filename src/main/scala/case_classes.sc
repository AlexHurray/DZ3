case class Product(id: Int, var category: String)

case class Order(id: Int, products: List[Product])

case class User(var name: String, var orders: List[Order]) {
  def isFavorite: Boolean = orders.size > 10
}

val user = User("Alexey", List(Order(1, List(Product(12, "Electronics")))))
user.isFavorite