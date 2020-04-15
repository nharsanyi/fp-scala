package com.practice.fp

object Chapter2 {


  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else go(n - 1, acc * n)
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

  def main(args: Array[String]): Unit = {
    println("5 factorial = %d".format(factorial(5)))
    println("1 factorial = %d".format(factorial(1)))
    println("3 factorial = %d".format(factorial(3)))

    println("3rd fibonacci = %d".format(fibo(3)))
    println("1st fibonacci = %d".format(fibo(1)))

  }
}
