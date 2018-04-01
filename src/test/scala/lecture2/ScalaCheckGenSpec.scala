package lecture2

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


/**
  * Created by a.a.perfilyev on 22.03.2018.
  */
object ScalaCheckGenSpec extends Properties("MyGen") {

  def genBoundedList(maxSize: Int, g: Gen[Int] = Gen.chooseNum(Int.MinValue, Int.MaxValue)): Gen[List[Int]] = {
    Gen.choose(0, maxSize) flatMap { sz => Gen.listOfN(sz, g) }
  }

  property("GenList") = forAll(genBoundedList(2)) { (xs: List[Int]) =>
    xs.size >= 0 && xs.size <= 2
  }
}