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
      else go(tail(l), i + 1)
    }

    go(l, 0)
  }

  //EX 3.5
  def dropWhile[A](l: FPList[A], f: A => Boolean): FPList[A] = {
    if (f(head(l))) dropWhile(tail(l), f)
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

  def foldRight[A, B](as: FPList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def prod2(ls: FPList[Double]) = {
    foldRight(ls, 1.0)(_ * _)
  }

  //EX 3.9
  def length[A](as: FPList[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  //EX 3.10
  def foldLeft[A, B](as: FPList[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
    }
  }

  def foldLeft2[A, B](as: FPList[A], z: B)(f: (B, A) => B): B = {
    def go(acc: B, as: FPList[A]): B = {
      as match {
        case Nil => acc
        case Cons(x, xs) => go(f(acc, x), xs)
      }
    }

    go(z, as)
  }

  //Ex 3.11
  def sumFL(is: FPList[Int]): Int = foldLeft2(is, 0)(_ + _)

  def productFL(is: FPList[Int]): Int = foldLeft2(is, 1)(_ * _)

  def lengthFL(is: FPList[Int]): Int = foldLeft2(is, 0)((x, y) => x + 1)

  //Ex 3.12
  def reverse[A](ls: FPList[A]): FPList[A] = {
    foldLeft2(ls, Nil: FPList[A])((x, y) => Cons(y, x))
  }

  //Ex 3.14
  def append[A](a1: FPList[A], a2: FPList[A]): FPList[A] = {
    //    foldLeft2(a2, a1)( (x, y) => Cons(y, x))
    foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  //Ex 3.15
  def flatList[A](ll: FPList[FPList[A]]): FPList[A] = {
    foldRight(ll, Nil: FPList[A])((x, y) => append(x, y))
  }

  //Ex 3.16
  def plusOne(ls: FPList[Int]): FPList[Int] = {
    ls match {
      case Nil => Nil
      case Cons(t, h) => Cons(t + 1, plusOne(h))
    }
  }

  //Ex 3.17
  def doubleToString(ls: FPList[Double]): FPList[String] = {
    ls match {
      case Nil => Nil
      case Cons(t, h) => Cons(t.toString + "a", doubleToString(h))
    }
  }

  //Ex 3.18
  def map[A, B](as: FPList[A])(f: A => B): FPList[B] = as match {
    case Nil => Nil
    case Cons(t, h) => Cons(f(t), map(h)(f))
  }

  //Ex 3.19
  def filter[A](as: FPList[A])(f: A => Boolean): FPList[A] = as match {
    case Nil => Nil
    case Cons(t, h) if f(t) => Cons(t, filter(h)(f))
    case Cons(_, h) => filter(h)(f)
  }

  //Ex 3.20
  def flatMap[A, B](as: FPList[A])(f: A => FPList[B]): FPList[B] = as match {
    case Nil => Nil
    case Cons(t, h) => append(f(t), flatMap(h)(f))
  }

  //Ex 3.21
  def filterFM[A](as: FPList[A])(f: A => Boolean): FPList[A] = as match {
    case Nil => Nil
    case Cons(t, h) if f(t) => flatMap(Cons(t, filterFM(h)(f)))(x => FPList(x))
    case Cons(_, h) => filterFM(h)(f)
  }

  //Ex 3.22
  def addLists(a1: FPList[Int], a2: FPList[Int]): FPList[Int] = a1 match {
    case Nil => Nil
    case Cons(t, h) => Cons(t + FPList.head(a2), addLists(h, FPList.tail(a2)))
  }

  //Ex 3.23
  def zipWith[A, B](a1: FPList[A], a2: FPList[B])(f: (A, B) => A): FPList[A] = a1 match {
    case Nil => Nil
    case Cons(t, h) => Cons(f(t, FPList.head(a2)), zipWith(h, FPList.tail(a2))(f))
  }

  def apply[A](as: A*): FPList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
