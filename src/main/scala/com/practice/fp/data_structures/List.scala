package com.practice.fp.data_structures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = head + ", " + tail
}


object List {

  /**
    * Removes elements from the List prefix as long as they match the predicate
    * @param list list
    * @param f predicate
    * @tparam A
    * @return
    */
  def dropwWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h)) dropwWhile(t, f)
      else list
    }
  }


  /**
    * Drops the first n element
    *
    * @param list
    * @param n the number of elements to remove from the head
    * @return
    * list without the first n element
    * if n is negative, throws Exception
    */
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n < 0) throw new IllegalArgumentException("Invalid parameter: n should be positive")
    else if (n == 0) list
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
