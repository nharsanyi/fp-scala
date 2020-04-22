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

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil // last element
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](list: List[A], x: B)(f: (A, B) => B): B = list match {
    case Nil => x
    case Cons(h, t) => f(h, foldRight(t, x)(f))
  }

  def foldLeft[A, B](list: List[A], x: B)(f: (B, A) => B): B = list match {
    case Nil => x
    case Cons(h, t) => foldLeft(t, f(x, h))(f)
  }

  def sumWithFoldLeft(list: List[Int]): Int = foldLeft(list, 0)(_ + _)
  def productWithFoldLeft(list: List[Double]): Double = foldLeft(list, 1.0)(_ * _)

  def lengthWithFoldRight[A](list: List[A]): Int = foldRight(list, 0)((_, l) => l + 1)
  def lengthWithFoldLeft[A](list: List[A]): Int = foldLeft(list, 0)((l, _) => l + 1)

  def reverse[A](list: List[A]): List[A] = foldLeft(list, List[A]())((rev, curr) => Cons(curr, rev))

//  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] = reverse(List.foldLeft(a2, reverse(a1))((res, curr) => Cons(curr, res)))
  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] = List.foldRight(a1, a2)(Cons(_, _))

  def concat[A](list: List[List[A]]): List[A] = List.foldRight(list, List[A]())(appendWithFold(_, _))

  def incrementElements(list: List[Int], incBy: Int): List[Int] = list match {
    case Nil => list
    case Cons(h, t) => Cons(h + incBy, incrementElements(t, incBy))
  }

  def convertToString(list: List[Double]): List[String] = list match {
    case Nil => List[String]()
    case Cons(h, t) => Cons(h.toString, convertToString(t))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => List[B]()
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => List[A]()
    case Cons(h, t) => {
      if (f(h)) {
        Cons(h, filter(t)(f))
      } else {
        filter(t)(f)
      }
    }
  }
}
