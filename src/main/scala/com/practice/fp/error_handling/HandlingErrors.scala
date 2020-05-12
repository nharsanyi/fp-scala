package com.practice.fp.error_handling


object HandlingErrors {

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case _: Exception => None}
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        a <- this
        bb <- b
      } yield f(a, bb)
    }

  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

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

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a match {
        case Nil => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      }
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

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def parseInts(a: List[String]): Option[List[Int]] = sequence(a.map(s => Try(s.toInt)))



}
