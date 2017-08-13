package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(v, heap)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two") = forAll {
    (v1: Int, v2: Int) =>
      findMin(insert(v2, insert(v1, empty))) == Math.min(v1, v2)
  }

  property("empty again") = forAll {
    (v: Int) =>
      isEmpty(deleteMin(insert(v, empty)))
  }

  property("ascending mins") = forAll {
    (h: H) =>
      def isAscending(oldMin: Int, heap: H): Boolean = {
        if (isEmpty(heap)) true // Reached the end
        else {
          val min = findMin(heap)
          (oldMin <= min) && isAscending(min, deleteMin(heap))
        }
      }
      isAscending(Int.MinValue, h)
  }

  property("melded mins") = forAll {
    (h1: H, h2: H) =>
      isEmpty(h1) || isEmpty(h2) ||
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("preserve values") = forAll {
    (vs: Seq[Int]) =>
      val filledHeap = vs.foldLeft(empty)((res, v) => insert(v, res))

      def extractValues(heap: H, vals: List[Int]): List[Int] = {
        if (isEmpty(heap)) vals
        else extractValues(deleteMin(heap), findMin(heap) :: vals)
      }

      vs.sorted == extractValues(filledHeap, Nil).sorted
  }
}
