package com.practice.fp.data_structures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def product(list: List[Int]): Int = list match {
    case Nil => 1
    case Cons(h, t) => h * product(t)
  }


  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }
}
