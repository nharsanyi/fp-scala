package com.practice.fp.laziness

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /**
    * Smart constructor - takes care of memoizing the by-name arguments
    * for the head and tail of the Cons
    *
    * @param hd
    * @param tl
    * @tparam A
    * @return
    */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd // thunks
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def toList[A](stream: Stream[A]): List[A] = {
    stream match {
      case Empty => List[A]()
      case Cons(h, t) => {
        h() :: toList(t())
      }
    }
  }

  def take[A](stream: Stream[A], n: Int): Stream[A] = {
    stream match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), take(t(), n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }
  }

  def drop[A](stream: Stream[A], n: Int): Stream[A] = {
    stream match {
      case Cons(_, t) if n > 0 => drop(t(), n - 1)
      case s => s
    }
  }

  def takeWhile[A](p: A => Boolean, stream: Stream[A]): Stream[A] = {
    stream match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), takeWhile(p, t()))
      case _ => Stream.empty
    }
  }

  def empty[A]: Stream[A] = Empty // returns empty, but annotates as Stream[A]
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
