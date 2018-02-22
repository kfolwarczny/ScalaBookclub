package chapter3

sealed trait FPList[+A]

case object Nil extends FPList[Nothing]

case class Cons[+A](head: A, tail: FPList[A]) extends FPList[A]


object FPList {
  def sum(ints: FPList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: FPList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def head[A](l: FPList[A]): A = {
    l match {
      case Nil => throw new IllegalArgumentException("List could not be Nil")
      case Cons(h, _) => h
    }
  }

  //EX 3.2
  def tail[A](l: FPList[A]): FPList[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  //EX 3.3
  def setHead[A](l: FPList[A], newVal: A): FPList[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => Cons(newVal, xs)
    }
  }

  //EX 3.4
  def drop[A](l: FPList[A], n: Int): FPList[A] = {
    if (n < 0) throw new IllegalArgumentException("n could not be below 0: " + n)

    def go(l: FPList[A], i: Int): FPList[A] = {
      if (i == n) l
      else if (l == Nil) Nil
      else go(FPList.tail(l), i + 1)
    }

    go(l, 0)
  }

  //EX 3.5
  def dropWhile[A](l: FPList[A], f: A => Boolean): FPList[A] = {
    if (f(FPList.head(l))) dropWhile(FPList.tail(l), f)
    else l
  }


  //EX 3.6
  def init[A](l: FPList[A]): FPList[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def apply[A](as: A*): FPList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
