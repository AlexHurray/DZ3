package lecture2

import org.scalatest.{BeforeAndAfter, GivenWhenThen, Matchers, WordSpec}

/**
  *
  *
  *
  */
class SortWordSpec extends WordSpec with Matchers with GivenWhenThen with BeforeAndAfter {
  //BeforeAndAfterAll

  before {
    // here we can put initialization of some static resources
    // or resources that are accessible by tests
  }

  after {
    // some finalization
  }

  val defaultSeq = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)


  "A seq" when {
    "shuffled" should {
      "not be equal to sorted seq" in {
        val shuffled = scala.util.Random.shuffle(defaultSeq)
        shuffled should not equal defaultSeq
      }
    }
    "sorted" should {
      "be equal to already sorted seq" in {
        val shuffled = scala.util.Random.shuffle(defaultSeq)
        shuffled should not equal defaultSeq

        val sorted = shuffled.sorted
        sorted shouldBe defaultSeq
      }

      "have the same size as original seq" in {
        val shuffled = scala.util.Random.shuffle(defaultSeq)
        shuffled should not equal defaultSeq

        val sorted = shuffled.sorted
        sorted should not equal shuffled
        sorted should have size shuffled.size
      }

      "throw exception" in {
        When("index requested exceed it's size")
        val shuffled = scala.util.Random.shuffle(defaultSeq)
        shuffled should not equal defaultSeq

        intercept[IndexOutOfBoundsException] {
          val sorted = shuffled.sorted
          sorted should not equal shuffled
          sorted(shuffled.length)
        }
      }
    }
  }
}

