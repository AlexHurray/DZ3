package lecture2

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object ScalaTestPermutationSpec extends Properties("MyGen") {
  def genBoundedList(maxSize: Int, g: Gen[Int] = Gen.chooseNum(Int.MinValue, Int.MaxValue)): Gen[List[Int]] = {
    Gen.choose(0, maxSize) flatMap { sz => Gen.listOfN(sz, g) }
  }

  /* Проверим нащу функцию отдельно на листах с гарантированными повторениями элементов */
  def genBoundedListWithDuplicates(maxSize: Int, g: Gen[Int] = Gen.chooseNum(Int.MinValue, Int.MaxValue)): Gen[List[Int]] = {
    Gen.choose(2, maxSize) flatMap { sz => Gen.listOfN(sz, Gen.oneOf(1 to sz - 1))
    }
  }

  def compare(actual: List[List[Int]], expected: List[Int]): Boolean = {
    actual.diff(expected.permutations.toList).isEmpty && expected.permutations.toList.diff(actual).isEmpty
  }

  val creator:PermutationsCreator = new PermutationsCreator

  property("GenList") = forAll(genBoundedList(8)) { (xs: List[Int]) =>
    compare(creator.getPermutaions(xs), xs)
  }

  property("GenListWithDupticates") = forAll(genBoundedListWithDuplicates(8)) { (xs: List[Int]) =>
    compare(creator.getPermutaions(xs), xs)
  }
}
