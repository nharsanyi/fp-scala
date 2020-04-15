package com.practice.fp

import scala.annotation.tailrec

/**
  * Getting started with functional programming in Scala
  */
object Chapter2 {


  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else go(n - 1, acc * n) // tail position -> calls it but does nothing with the result
    }

    go(n, 1)
  }

  def fibo(n: Int): Int = {
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n <= 0) prev
      else go(n - 1, curr, prev + curr)
    }

    go(n, 0, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    "The %s of %d is %d".format(name, n, f(n))
  }

  /**
    * polymorphic function to find the first element of an array that matches the given condition
    * @param arr array
    * @param p condition to match
    * @tparam A
    * @return index of the element, returns -1 if no element found
    */
  def findFirst[A](arr: Array[A], p: A => Boolean): Int = {

    @tailrec
    def go(pos: Int): Int = {
      if (pos >= arr.length) -1
      else if (p(arr(pos))) pos
      else go(pos + 1)
    }
    go(0)
  }

  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(index: Int):Boolean = {
      if (index == arr.length - 1) true
      else if (!ordered(arr(index), arr(index + 1))) false
      else go(index + 1)
    }
    go(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 5, factorial))
    println(formatResult("fibonacci", 5, fibo))
    println("First index of an even number is %d".format(findFirst[Int](Array(1, 3, 5, 2), x => x % 2 == 0)))
    println("First index of an uppercase string is %d".format(findFirst[String](Array("Apple", "Banana", "LEMON", "peaR"), x => x.matches("[A-Z]+"))))

    println(isSorted[Int](Array(1,3,5,7), (x, y) => x <= y))
    println(isSorted[Int](Array(1,3,8,7), (x, y) => x <= y))
  }
}
