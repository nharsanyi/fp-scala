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

    assert(List.product(l) == 12)
    assert(List.product(emptyList) == 1)
    assert(List.product(oneElementList) == 2)
  }

}
