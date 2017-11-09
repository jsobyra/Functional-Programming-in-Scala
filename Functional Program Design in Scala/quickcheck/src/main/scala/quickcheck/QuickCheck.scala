package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a;
  }

  property("min2") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a1, insert(a2, empty))
    val min = findMin(h);
    if(a1 < a2) min == a1
    else min == a2
  }

  property("empty1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val newH = deleteMin(h)
    isEmpty(newH)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val smallest = if(min1 < min2) min1 else min2
    val h = meld(h1, h2)
    findMin(h) == smallest
  }

  property("remMin1") = forAll { (h: H) =>
    val sortedList = remMin(h, Nil)
    sortedList == sortedList.sorted
  }

  property("meldRemMove") = forAll { (h1: H, h2: H) =>
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))

    val remMin1 = remMin(meld1, Nil)
    val remMin2 = remMin(meld2, Nil)

    remMin1 == remMin2
  }

  def remMin(hs: H, sequence: List[Int]): List[Int] = {
    if(isEmpty(hs)) sequence
    else findMin(hs) :: remMin(deleteMin(hs), sequence)
  }
}
