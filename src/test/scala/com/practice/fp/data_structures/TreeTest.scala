package com.practice.fp.data_structures

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("should return size") {
    val tree1 = new Branch[Int](new Leaf[Int](5), new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)))
    assertResult(5) (tree1.size())

    val tree2 = new Branch[Int](new Leaf[Int](1), new Leaf[Int](2))
    assertResult(3) (tree2.size())

    val tree3 = new Leaf[Int](3)
    assertResult(1) (tree3.size())
  }

  test("should return max element of the tree") {
    val tree1 = new Branch[Int](new Leaf[Int](5), new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)))
    assertResult(5)(Tree.max(tree1))

    val tree2 = new Branch[Int](new Leaf[Int](1), new Leaf[Int](2))
    assertResult(2)(Tree.max(tree2))

    val tree3 = new Leaf[Int](3)
    assertResult(3)(Tree.max(tree3))
  }

}
