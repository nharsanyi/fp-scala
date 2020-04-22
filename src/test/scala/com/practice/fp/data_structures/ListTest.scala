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

  test("test tail") {
    val list = List(1, 2, 3)
    assert(List.tail(list) == List(2, 3))

    val emptyList = List()
    assert(List.tail(emptyList) == List())

    val oneElementList = List(5)
    assert(List.tail(oneElementList) == List())
  }

  test("test setHead") {
    val list = List(1, 2, 3)
    assert(List.setHead(5, list) == List(5, 2, 3))

    val emptyList = List()
    assertThrows[IllegalArgumentException](List.setHead(2, emptyList))

    val oneElementList = List(5)
    assert(List.setHead(2, oneElementList) == List(2))
  }

  test("should drop") {
    val list = List(1, 2, 3)
    assert(List.drop(list, 2) == List(3))

    val emptyList = List()
    assert(List.drop(emptyList, 3) == List())

    val list2 = List(1, 2, 3, 4)
    assertResult(List())(List.drop(list2, 4))

    val list3 = List(1, 2, 3, 4)
    assertResult(List())(List.drop(list3, 6))

    val list4 = List(1, 2)
    assertThrows[IllegalArgumentException](List.drop(list4, -2))
  }

  test("should dropWhile") {
    val list = List(-1, -3, 5, -2)
    assertResult(List(5, -2))(List.dropwWhile(list, (x: Int) => x < 0))

    val emptyList = List[String]()
    assertResult(List())(List.dropwWhile(emptyList, (x: String) => x.startsWith("X")))

    val list2 = List(3, 5, 6)
    assertResult(List(3, 5, 6))(List.dropwWhile(list2, (x: Int) => x < 0))

    val list3 = List(-3)
    assertResult(List())(List.dropwWhile(list3, (x: Int) => x < 0))

    val list4 = List(3)
    assertResult(List(3))(List.dropwWhile(list4, (x: Int) => x < 0))
  }

  test("should append") {
    assertResult(List(1, 2, 3, 4, 5, 6))(List.append(List(1, 2, 3), List(4, 5, 6)))
    assertResult(List(1, 2, 3))(List.append(List(1, 2, 3), List()))
    assertResult(List(1, 2, 3))(List.append(List(), List(1, 2, 3)))
    assertResult(List())(List.append(List(), List()))
  }

  test("should init") {
    assertResult(List(1, 2))(List.init(List(1, 2, 3)))
    assertResult(List())(List.init(List(1)))
    assertResult(List())(List.init(List()))
  }

  test("assert foldRight") {
    assertResult(5)(List.foldRight(List(2, 3), 0)(_ + _))
    assertResult(5)(List.foldRight(List(1, 5), 1.0)(_ * _))
    assertResult(List(1, 2, 3))(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_ , _)))
  }

  test("assert sumWithFoldLeft") {
    assertResult(5)(List.sumWithFoldLeft(List(1, 2, 2)))
    assertResult(0)(List.sumWithFoldLeft(List()))
    assertResult(3)(List.sumWithFoldLeft(List(3)))
  }

  test("assert productWithFoldLeft") {
    assertResult(6)(List.productWithFoldLeft(List(1, 2, 3)))
    assertResult(0)(List.productWithFoldLeft(List(1, 2, 0)))
    assertResult(1)(List.productWithFoldLeft(List(1)))
  }

  test("assert length") {
    assertResult(3)(List.lengthWithFoldRight(List(1, 2, 3)))
    assertResult(3)(List.lengthWithFoldLeft(List(1, 2, 3)))
    assertResult(0)(List.lengthWithFoldRight(List()))
    assertResult(0)(List.lengthWithFoldLeft(List()))
    assertResult(1)(List.lengthWithFoldRight(List(1)))
    assertResult(1)(List.lengthWithFoldLeft(List(1)))
  }

  test("assert reverse") {
    assertResult(List(3, 2, 1))(List.reverse(List(1, 2, 3)))
    assertResult(List())(List.reverse(List()))
    assertResult(List(1))(List.reverse(List(1)))
  }

  test("assert appendWithFold") {
    assertResult(List(1, 2, 3, 4, 5))(List.appendWithFold(List(1, 2, 3), List(4, 5)))
    assertResult(List(1, 2, 3))(List.appendWithFold(List(), List(1, 2, 3)))
    assertResult(List(1, 2, 3))(List.appendWithFold(List(1, 2, 3), List()))
    assertResult(List())(List.appendWithFold(List(), List()))
  }

  test("assert concat") {
    assertResult(List(1, 2, 3, 4, 5))(List.concat(List(List(1, 2), List(3), List(4, 5))))
    assertResult(List(1, 2, 3, 4, 5))(List.concat(List(List(), List(1, 2, 3, 4, 5))))
    assertResult(List())(List.concat(List(List(), List())))
    assertResult(List(1))(List.concat(List(List(), List(1))))
    assertResult(List(1))(List.concat(List(List(1), List())))
  }

  test("assert incrementElements") {
    assertResult(List(2, 3, 4, 5))(List.incrementElements(List(1, 2, 3, 4), 1))
    assertResult(List(3))(List.incrementElements(List(2), 1))
  }

  test("assert convertToString") {
    assertResult(List("1.0", "2.0", "3.0"))(List.convertToString(List(1, 2, 3)))
    assertResult(List("1.0"))(List.convertToString(List(1)))
    assertResult(List())(List.convertToString(List()))
  }

  test("assert map") {
    val f: Int => Int = (x: Int) => x * 2
    assertResult(List(2, 4, 6))(List.map(List(1, 2, 3))(f))
  }
}
