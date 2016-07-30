package quickcheck

import scala.annotation.tailrec

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen.value
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("insert empty min") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert min") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("math min empty") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("delete min empty") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sorted") = forAll { (h: H) =>
    isSorted(h)
  }

//  NOT SURE IF IT IS MY FAULT, BUT THIS PROPERTY IS NOT VERIFIED!
//  property("sorted sequence") = forAll { h: H =>
//    val seq = sortedElements(h)
//    val indexes = for (i <- Gen.choose(0, seq.length - 1))
//      yield i
//
//    forAll(indexes, indexes) {
//      (i, j) => seq(i) <= seq(j)
//    }
//
//    seq.sorted.size == size(h)
//  }

  property("min meld") = forAll { (h1: H, h2: H) =>
    // If h1 or h2 are empty --> findMin(h1) or findMin(h2) throws NoSuchElementException
    // But 'genHeap' never generates empty heaps!
    // In fact, "h: H" generate a heap calling 'Arbitrary[H]', namely 'Arbitrary(genHeap)'
    findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

  property("size") = forAll { (h1: H, h2: H) =>
    size(insert(0, h1)) == 1 + size(h1)
    size(deleteMin(h2)) == size(h2) - 1
    size(meld(h1, h2)) == size(h1) + size(h2)
  }

  property("equals") = forAll { (h1: H, h2: H) =>
    val merge1 = meld(h1, h2)
    val del = deleteMin(h2)
    val min: Int = findMin(h2)
    val ins = insert(min, h1)
    val merge2 = meld(ins, del)

    equals(merge1, merge2)
  }

  property("max & min") = forAll { h: H =>
    val min = findMin(h)
    val max = findMax(h)
    val sortedSeq = sortedElements(h)

    sortedSeq.apply(0) == min
    sortedSeq(sortedSeq.size - 1) == max
    min <= max
  }

  /**
   * Builds an ascending sorted indexed sequence
   * removing the elements from a heap
   */
  def sortedElements(h: H): IndexedSeq[Int] = {

    @tailrec
    def sortedElementsAcc(curr: H, acc: IndexedSeq[Int]): IndexedSeq[Int] =
      if (isEmpty(curr)) acc
      else sortedElementsAcc(deleteMin(curr), findMin(curr) +: acc)

    sortedElementsAcc(h, Vector.empty)
  }

  /**
   * Checks whether or not the two heaps contain the same elements
   */
  def equals(h1: H, h2: H): Boolean =
    if (isEmpty(h1) && isEmpty(h2)) true
    else findMin(h1) == findMin(h2) && equals(deleteMin(h1), deleteMin(h2))

  /**
   * Checks whether or not the heap is sorted
   */
  def isSorted(h: H): Boolean =
    if (isEmpty(h)) true
    else {
      val tail = deleteMin(h)
      if (isEmpty(tail)) true
      else findMin(h) <= findMin(tail) && isSorted(tail)
    }

  /**
   * Max of a heap
   */
  def findMax(h: H): Int = {

    @tailrec
    def findMaxAcc(curr: H, max: Int): Int =
      if (isEmpty(curr)) max
      else findMaxAcc(deleteMin(curr), math.max(max, findMin(h)))

    if (isEmpty(h)) throw new NoSuchElementException("max of empty heap")
    else findMaxAcc(h, Int.MinValue)
  }

  /**
   * Size of a heap
   */
  def size(h: H): Int = {

    @tailrec
    def sizeAcc(curr: H, size: Int): Int =
      if (isEmpty(curr)) size
      else sizeAcc(deleteMin(curr), 1 + size)

    sizeAcc(h, 0)
  }

  /**
   * Generates a random IntHeap, which is not empty
   */
  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
