package com.practice.fp.laziness

class MyStream {

  sealed trait Stream[+A] {

    def toList: List[A] = {
      this match {
        case Empty => List[A]()
        case Cons(h, t) => {
          h() :: t().toList
        }
      }
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    /**
      * Smart constructor - takes care of memoizing the by-name arguments
      * for the head and tail of the Cons
      * @param hd
      * @param tl
      * @tparam A
      * @return
      */
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd // thunks
      lazy val tail = tl
      Cons(()  => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty // returns empty, but annotates as Stream[A]
    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
