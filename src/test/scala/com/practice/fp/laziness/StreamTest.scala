package com.practice.fp.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("should convert stream to list") {
    assertResult(List(1, 2, 3))(Stream.toList(Stream(1, 2, 3)))
    assertResult(List())(Stream.toList(Stream()))
  }

  test("should take first n element") {
    assertResult(List(1, 2, 3))(Stream.toList(Stream.take(Stream(1, 2, 3, 4, 5), 3)))
    assertResult(List(1))(Stream.toList(Stream.take(Stream(1, 2, 3, 4, 5), 1)))
    assertResult(List())(Stream.toList(Stream.take(Stream(1, 2, 3, 4, 5), 0)))
  }
}
