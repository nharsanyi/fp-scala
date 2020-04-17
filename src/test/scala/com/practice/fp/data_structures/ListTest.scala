package com.practice.fp.data_structures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("assert sum") {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    val emptyList = Nil
    val oneElementList = Cons(2, Nil)

    assert(List.sum(l) == 6)
    assert(List.sum(emptyList) == 0)
    assert(List.sum(oneElementList) == 2)
  }

  test("assert product") {
    val l = Cons(1, Cons(2, Cons(3, Cons(2, Nil))))
    val emptyList = Nil
    val oneElementList = Cons(2, Nil)
    val containsZero = Cons(2, Cons(0, Cons(3, Nil)))

    assert(List.product(l) == 12)
    assert(List.product(emptyList) == 1)
    assert(List.product(oneElementList) == 2)
    assert(List.product(containsZero) == 0)
  }

  test("assert apply") {
    val l = List("a", "b", "c")
    assert(l.toString == "a, b, c, Nil")

    val emptyList = List()
    assert(emptyList.toString == "Nil")

    val oneElementList = List(1.3d)
    assert(oneElementList.toString == "1.3, Nil")
  }

  test("pattern matching test") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

}
