package quickcheck

import java.util.NoSuchElementException

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v: Int <- arbitrary[Int]
    m: H <- oneOf(const(empty), genHeap)
    h: H <- Gen.oneOf(empty, insert(v, m))
  } yield h


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  val ints = Arbitrary.arbitrary[Int]

  //  implicit lazy val arbInt: Arbitrary[Int] = Arbitrary(oneOf(0,1))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("insert two element") = forAll { (h1: H, h2: H) =>
    (!(isEmpty(h1) && isEmpty(h2))) ==> {
      if (isEmpty(h1) && !isEmpty(h2)) {
        findMin(h2) == findMin(meld(h1, h2))
      } else if (isEmpty(h2) && !isEmpty(h1)) {
        findMin(h1) == findMin(meld(h1, h2))
      } else {
        val m = findMin(h1)
        val n = findMin(h2)
        val l = findMin((meld(h1, h2)))
        if (m <= n) m == l
        else n == l
      }
    }
  }

  property("insert two empty heap") = forAll { (h1: H, h2: H) =>
    (isEmpty(h1) && isEmpty(h2)) ==> {
      meld(h1, h2) == empty
    }
  }

  property("insert two empty heap") = forAll { (h: H) =>
    (!isEmpty(h)) ==> {
      meld(h, empty) == h && meld(empty, h) == h
    }
  }

  property("delete element") = forAll { (v: Int) =>
    val h1 = insert(v, empty)
    val h = deleteMin(h1)
    h == empty
  }


  property("insert the same element twice") = forAll { (v: Int) =>
    val h1 = insert(v, insert(v, empty))
    length(h1) == 2
    deleteMin(deleteMin(h1)) == empty
  }

  property("insert two element into empty heap") = forAll { (v1: Int, v2: Int) =>
    val h1 = insert(v1, empty)
    val h2 = insert(v2, h1)
    length(h1) == 2
    deleteMin(deleteMin(h2)) == empty
  }

  property("check length") = forAll { (v: H) =>
    (!isEmpty(v)) ==> {
      length(v) == 1 + length(deleteMin(v))
    }
  }

  property("check ordered") = forAll { (v: H) =>
    ordered(v)
  }


  property("check empty") = forAll { v: H =>
    (isEmpty(v)) ==> {
      try {
        val num = findMin(v)
        falsified
      } catch {
        case e => e.isInstanceOf[NoSuchElementException]
      }
    }
  }


  property("length of two heap") = forAll { (h1: H, h2: H) =>
    length(h1) + length(h2) == length(meld(h1, h2))
  }

  def length(h: H): Int = {
    if (isEmpty(h)) 0
    else {
      1 + length(deleteMin(h))
    }
  }


  def ordered(h: H): Boolean = h match {
    case empty => {
      true
    }
    case _ => {
      val min = findMin(h)
      val next = deleteMin(h)
      if (isEmpty(next))
        true
      else
        min <= findMin(next) && ordered(next)
    }
  }


}
