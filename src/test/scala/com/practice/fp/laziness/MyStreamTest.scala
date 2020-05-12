package com.practice.fp.laziness

import org.scalatest.FunSuite

class MyStreamTest extends FunSuite {

  test("should convert stream to list") {
    assertResult(List(1, 2, 3))(new MyStream().Stream(1, 2, 3).toList)
    assertResult(List())(new MyStream().Stream().toList)
  }
}
