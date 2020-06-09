// This is not a perfect solution for the quickcheck assignment
package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[Int]
      n <- oneOf(const(empty), genHeap)
    } yield insert(a, n)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Insert two elements, min is the smaller element") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) ==  (if (a < b) a else b)
  }

  property("delete single element heap should be empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("Heaps should be sorted") = forAll { h: H =>
    def getList (h: H): List[A] =
      if (isEmpty(h)) List() else findMin(h) :: getList(deleteMin(h))
    val l = getList(h)
    val sorted = l.sorted
    sorted == l
  }


  property("meld min = min of one") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => isEmpty(m)
      case (true, false) => findMin(h2) == findMin(m)
      case (false, true) => findMin(h1) == findMin(m)
      case (false, false) => findMin(m) == findMin(h1) || findMin(m) == findMin(h2)
    }
  }

  property("Removed heap will have shorter length") = forAll { (h: H, a:Int) =>
    val h_ins = insert(a, h)
    def getList (h: H): List[A] =
      if (isEmpty(h)) List() else findMin(h) :: getList(deleteMin(h))
    val l = getList(h)
    getList(h_ins).length > getList(deleteMin(h_ins)).length
  }

  property("Melded Heaps should be sorted") = forAll { (h1: H, h2: H) =>
    def getList (h: H): List[A] =
      if (isEmpty(h)) List() else findMin(h) :: getList(deleteMin(h))
    val m = meld(h1,h2)
    val l = getList(m)
    l.sorted == l
  }

  property("After delete min, should still be sorted") = forAll {(h: H, a: Int) =>
    def getList (h: H): List[A] =
      if (isEmpty(h)) List() else findMin(h) :: getList(deleteMin(h))
    val h_ins = insert(a, h)
    val h2 = deleteMin(h_ins)
    val l = getList(h2)
    l.sorted == l

  }

  property("findMin on deleted melded heaps of 1 element should equal to min of the two elements") = forAll{ (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val melded = meld(h1,h2)
    findMin(deleteMin(melded)) == (a::b::Nil).min
  }

}
