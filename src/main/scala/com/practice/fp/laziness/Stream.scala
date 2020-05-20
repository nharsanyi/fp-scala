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

  def foldRight[A, B](z: => B)(f: (A, B) => B, stream: Stream[A]): B = {
    stream match {
      case Cons(h, t) => f(h(), foldRight(z)(f, t()))
      case _ => z
    }
  }

  def forAll[A](p: A => Boolean, stream: Stream[A]): Boolean = {
    stream match {
      case Cons(h, t) => p(h()) && forAll(p, t())
      case _ => true
    }
  }

  def takeWhileWithFold[A](p: A => Boolean, stream: Stream[A]): Stream[A] = {
    foldRight[A, Stream[A]](empty[A])((h, t) => if (p(h)) cons(h, t) else empty[A], stream)
  }

  def headOption[A](stream: Stream[A]): Option[A] = {
    foldRight[A, Option[A]](None: Option[A])((h, _) => Some(h), stream)
  }

  def map[A, B](f: A => B, stream: Stream[A]): Stream[B] = {
    foldRight[A, Stream[B]](empty[B])((h, t) => cons(f(h), t), stream)
  }

  def filter[A](f: A => Boolean, stream: Stream[A]): Stream[A] = {
    foldRight[A, Stream[A]](empty[A])((h, t) => if (f(h)) cons(h, t) else t, stream)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def fibs(): Stream[Int] = {

    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /**
    *
    * @param z: Initial state
    * @param f: function to produce the next state and the next value
    * @tparam A
    * @tparam S
    * @return
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fibsWithUnfold() = {
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1)))}
  }

  def fromWithUnfold(n: Int) = {
    unfold(n) { case a => Some((a, a + 1)) }
  }

  def constantWithUnfold(n: Int) = {
    unfold(n) {case a => Some((a, a))}
  }

  def empty[A]: Stream[A] = Empty // returns empty, but annotates as Stream[A]
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
