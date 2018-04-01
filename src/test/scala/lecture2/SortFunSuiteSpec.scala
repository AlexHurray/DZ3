package lecture2

import org.scalatest.{ParallelTestExecution, FunSuite, Matchers}

class SortFunSuiteSpec extends FunSuite with Matchers {

  val defaultSeq = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)

  test("shuffled seq should not equal sorted seq") {
    val shuffled = scala.util.Random.shuffle(defaultSeq)
    shuffled should not equal defaultSeq
  }

  test("a sorted seq should equal to already sorted seq") {
    val shuffled = scala.util.Random.shuffle(defaultSeq)
    shuffled should not equal defaultSeq

    val sorted = shuffled.sorted
    sorted shouldBe defaultSeq
  }

  test("a size of a sorted seq should be the same") {
    val shuffled = scala.util.Random.shuffle(defaultSeq)
    shuffled should not equal defaultSeq

    val sorted = shuffled.sorted
    sorted should have size shuffled.size
  }

  test("an error should occur if index exceed seq size") {
    val shuffled = scala.util.Random.shuffle(defaultSeq)
    shuffled should not equal defaultSeq
    intercept[IndexOutOfBoundsException] {
      val sorted = shuffled.sorted
      sorted(shuffled.length)
    }
  }
}
