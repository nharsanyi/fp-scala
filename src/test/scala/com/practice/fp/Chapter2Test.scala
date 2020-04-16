package com.practice.fp

import com.practice.fp.Chapter2._
import org.scalatest.FunSuite

class Chapter2Test extends FunSuite {

  test("assert isSorted") {
    assert(isSorted(Array[Int](1, 2, 3, 6), (x: Int, y: Int) => x <=y))
    assert(!isSorted(Array[Int](1, 6, 3, 6), (x: Int, y: Int) => x <=y))
    assert(isSorted(Array[Int](1, 1, 3, 6), (x: Int, y: Int) => x <=y))
  }

  test("assert factorial") {
    assert(factorial(5) == 120)
    assert(factorial(1) == 1)
  }

  test("assert fibonacci") {
    assert(fibo(5) == 5)
    assert(fibo(1) == 1)
  }

  test("assert findFirst") {
    assert(findFirst[Int](Array(1, 3, 5, 2), x => x % 2 == 0) == 3)
    assert(findFirst[String](Array("Apple", "Banana", "LEMON", "peaR"), x => x.matches("[A-Z]+")) == 2)
  }

  test("assert curry") {
    def f(a: Int, b: Int): String = "%d_%d".format(a, b)
    def g(a: Int) (b: Int): String = "%d_%d".format(a, b)

    assert(curry(f)(1)(1) == f(1, 1))
    assert(curry(f)(1)(1) == g(1)(1))
  }

  test("assert uncurry") {
    def f(a: Int, b: Int): String = "%d_%d".format(a, b)
    def g(a: Int) (b: Int): String = "%d_%d".format(a, b)

    assert(uncurry(g)(1, 1) == g(1)(1))
    assert(uncurry(g)(1, 1) == f(1, 1))
  }

}
