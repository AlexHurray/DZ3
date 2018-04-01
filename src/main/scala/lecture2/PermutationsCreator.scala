package lecture2

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class PermutationsCreator {

  def swap(ab: ArrayBuffer[Int], i: Int, j: Int) = {
    val s: Int = ab(i)
    ab(i) = ab(j)
    ab(j) = s
  }

  def getNextPermutation(list: List[Int]): List[Int] = {
    val ab: ArrayBuffer[Int] = ArrayBuffer(list: _*)

    var j = ab.length - 2

    while (j >= 0 && ab(j) >= ab(j + 1))
      j = j - 1

    if (j < 0)
      List.empty
    else {
      var k = ab.length - 1

      while (ab(j) >= ab(k))
        k = k - 1

      swap(ab, j, k)

      var l = j + 1
      var r = ab.length - 1

      while (l < r) {
        swap(ab, l, r)
        l = l + 1
        r = r - 1
      }

      ab.toList
    }
  }

  def getPermutaions(list: List[Int]): List[List[Int]] = {
    val lb: ListBuffer[List[Int]] = ListBuffer.empty
    lb.append(list.sorted)
    var newList: List[Int] = List.empty

    do {
      newList = getNextPermutation(lb.last)
      if (newList.nonEmpty)
        lb.append(newList)
    } while (newList.nonEmpty)

    lb.toList
  }
}
