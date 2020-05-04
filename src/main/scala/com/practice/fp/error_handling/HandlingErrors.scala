package com.practice.fp.error_handling

object HandlingErrors {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(_) => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if (f(a)) =>  this
      case _ => None
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b map (bb => f(aa, bb)))
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      val mean = xs.sum / xs.size
      val r = xs.map(x => math.pow(x - mean, 2))
      Some(r.sum / r.size)
    }
  }

}
