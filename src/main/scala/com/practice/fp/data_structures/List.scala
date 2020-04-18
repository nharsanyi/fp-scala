package com.practice.fp.data_structures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = head + ", " + tail
}


object List {

  def drop(list: List[Int], n: Int): List[Int] = {
    if (n == 0) list
    else {
      list match {
        case Cons(_, t) => drop(t, n - 1)
        case Nil => Nil
      }
    }
  }


  def product(list: List[Int]): Int = list match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(h, t) => h * product(t)
  }


  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](newHead: A, list: List[A]) = list match {
    case Nil => throw new IllegalArgumentException("Set head on empty list is not allowed")
    case Cons(_, t) => Cons(newHead, t)
  }

}
