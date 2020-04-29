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

  test("should return the maximum path from the root the any leaf") {
    val tree1 = new Branch[Int](new Leaf[Int](5), new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)))
    assertResult(2)(Tree.depth(tree1))

    val tree2 = new Branch[Int](new Leaf[Int](1), new Leaf[Int](2))
    assertResult(1)(Tree.depth(tree2))

    val tree3 = new Leaf[Int](3)
    assertResult(0)(Tree.depth(tree3))
  }


  test("should map tree to another tree") {
    val tree1 = new Branch[Int](new Leaf[Int](5), new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)))
    assertResult(new Branch[Int](new Leaf[Int](10), new Branch[Int](new Leaf[Int](6), new Leaf[Int](8))))(Tree.map(tree1)(_ * 2))

    val tree2 = new Branch[Int](new Leaf[Int](1), new Leaf[Int](2))
    assertResult(new Branch[Int](new Leaf[Int](2), new Leaf[Int](3)))(Tree.map(tree2)(_ + 1))

    val tree3 = new Leaf[Int](3)
    assertResult(new Leaf[String]("3_a"))(Tree.map(tree3)(x => "%d_a".format(x)))
  }

  test("should assert fold") {
    val tree1 = new Branch[Int](new Leaf[Int](5), new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)))
    // max
    assertResult(5)(Tree.fold[Int, Int](tree1)(x => x)((x, y) => x.max(y)))
    // depth
    assertResult(2)(Tree.fold[Int, Int](tree1)(_ => 0)((x, y) => 1 + x.max(y)))
    // map
    val mapF: Int => Int = (x: Int) => x * 2
    val expectedTree: Tree[Int] = new Branch[Int](new Leaf[Int](10), new Branch[Int](new Leaf[Int](6), new Leaf[Int](8)))
    val actualTree: Tree[Int] = Tree.fold(tree1)(x => Leaf(mapF(x)): Tree[Int])(Branch(_, _))
    assertResult(expectedTree)(actualTree)
  }
}
