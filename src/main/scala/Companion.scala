import java.util.concurrent.atomic.AtomicInteger

class Companion(id: Int) {
  def greeting = println(s"Hello I'm $id of ${Companion.counter}")
}
object Companion {
  def apply() = new Companion(counter.incrementAndGet())
  var counter: AtomicInteger = new AtomicInteger(0)
}