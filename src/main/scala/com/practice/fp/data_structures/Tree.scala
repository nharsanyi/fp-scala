package com.practice.fp.data_structures

sealed trait Tree[+A] {
  def size(): Long = {
    this match {
      case (Leaf(_)) => 1
      case Branch(l, r) => 1 + l.size() + r.size()
    }
  }
}

case class Leaf[A](value: A) extends Tree[A] {
  override def toString: String = s"Leaf($value)"
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def toString: String = s"Branch(left=$left, right=$right)"
}

object Tree {

  def max(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => max(l).max(max(r))
    }
  }

  def depth[A](tree: Tree[A]): Long = {
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => new Branch[B](map(l)(f), map(r)(f))
    }
  }
}
