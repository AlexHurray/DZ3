import org.scalatest.FlatSpec

class PermutationsSpec extends FlatSpec {

  val testedFunc: List[Int] => List[List[Int]] = ???

  def compare(actual: List[List[Int]], expected: List[Int]): Boolean = {
    actual.diff(expected.permutations.toList).isEmpty && expected.permutations.toList.diff(actual).isEmpty
  }

  "My permutation func" should "work right for empty list" in {
    val xs = List.empty[Int]
    assert(compare(testedFunc(xs), xs))
  }

}
