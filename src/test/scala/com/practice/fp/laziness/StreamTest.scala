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

  test("should return headOption") {
    assertResult(Some(1))(Stream.headOption(Stream(1, 2, 3)))
    assertResult(None)(Stream.headOption(Stream()))
  }

  test("should map stream to another stream") {
    assertResult(List(2, 4, 6))(Stream.toList(Stream.map((x: Int) => x * 2, Stream(1, 2, 3))))
    assertResult(List())(Stream.toList(Stream.map((x: Int) => x * 2, Stream())))
  }

  test("should filter stream") {
    assertResult(List(2, 4))(Stream.toList(Stream.filter((x: Int) => x % 2 == 0, Stream(1, 2, 3, 4))))
    assertResult(List())(Stream.toList(Stream.filter((x: Int) => x < 0, Stream(1, 2, 3, 4))))
    assertResult(List())(Stream.toList(Stream.filter((x: Int) => x < 0, Stream())))
  }

  test("should create constant stream") {
    assertResult(List(2, 2, 2, 2))(Stream.toList(Stream.take(Stream.constant(2), 4)))
  }

  test("should create stream from N") {
    assertResult(List(51, 52, 53))(Stream.toList(Stream.take(Stream.from(51), 3)))
  }

  test("should generate fibonacci stream") {
    assertResult(List(0, 1, 1, 2, 3, 5, 8))(Stream.toList(Stream.take(Stream.fibs(), 7)))
  }

  test("should generate fibonacci stream with unfold") {
    assertResult(List(0, 1, 1, 2, 3, 5, 8))(Stream.toList(Stream.take(Stream.fibsWithUnfold(), 7)))
  }

  test("should create stream from N with unfold") {
    assertResult(List(51, 52, 53))(Stream.toList(Stream.take(Stream.fromWithUnfold(51), 3)))
  }

  test("should create constant stream with unfold") {
    assertResult(List(2, 2, 2, 2))(Stream.toList(Stream.take(Stream.constantWithUnfold(2), 4)))
  }

}
