package com.practice.fp.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("should convert stream to list") {
    assertResult(List(1, 2, 3))(Stream.toList(Stream(1, 2, 3)))
    assertResult(List())(Stream.toList(Stream()))
  }

  test("should take the first n element") {
    assertResult(List(1, 2, 3))(Stream.toList(Stream.take(Stream(1, 2, 3, 4, 5), 3)))
    assertResult(List(1))(Stream.toList(Stream.take(Stream(1, 2, 3, 4, 5), 1)))
    assertResult(List())(Stream.toList(Stream.take(Stream(1, 2, 3, 4, 5), 0)))
  }

  test("should take while elements for the given predicate are true") {
    assertResult(List(1, 2, 3))(Stream.toList(Stream.takeWhile( (x: Int) => x < 4, Stream(1, 2, 3, 4, 5))))
    assertResult(List())(Stream.toList(Stream.takeWhile( (x: Int) => x < 2, Stream(4, 1, 2, 5))))
  }

  test("should drop the first n element") {
    assertResult(List(4, 5))(Stream.toList(Stream.drop(Stream(1, 2, 3, 4, 5), 3)))
    assertResult(List(2, 3, 4, 5))(Stream.toList(Stream.drop(Stream(1, 2, 3, 4, 5), 1)))
    assertResult(List(1, 2, 3, 4, 5))(Stream.toList(Stream.drop(Stream(1, 2, 3, 4, 5), 0)))
  }

  test("should check all elements in the list") {
    assert(Stream.forAll((x: Int) => x < 4, Stream(1, 2, 3)))
    assert(!Stream.forAll((x: Int) => x % 2 == 0, Stream(1, 2, 3)))
  }

  test("should take while elements for the given predicate are true (foldRight impl)") {
    assertResult(List(1, 2, 3))(Stream.toList(Stream.takeWhileWithFold( (x: Int) => x < 4, Stream(1, 2, 3, 4, 5))))
    assertResult(List())(Stream.toList(Stream.takeWhileWithFold( (x: Int) => x < 2, Stream(4, 1, 2, 5))))
  }

}
