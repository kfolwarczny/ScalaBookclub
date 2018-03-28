package chapter4


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  /*  def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(x) => Some(f(x)).get
      case _ => None
    }*/

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(x => Some(x)).getOrElse(ob)
  }

  /*  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(x) => Some(x)
      case _ => ob
    }*/

  //RAFAL
/*  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this != None) this
    else ob
  }*/

  //PATRYK
  /*def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.flatMap(a => ob)
  }*/

  def filter(f: A => Boolean): Option[A] = {
    if (this.map(f).getOrElse(false)) this
    else None
  }

  /*  def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) if f(x) => Some(x)
      case _ => None
    }*/

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, None) => None
      case (_, None) => None
      case (None, _) => None
      case (Some(x), Some(y)) => Some(f(x, y))
    }
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => Some(xs.map(x => Math.pow(x - m, 2)).sum / xs.length))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(aa => aa)

     val as: List[A] = a.foldLeft(Nil: List[A])((x, y) => x ::: y.map(yy => List(yy)).getOrElse(Nil: List[A]))

     as match {
       case Nil => None
       case x :: tail if (x :: tail).lengthCompare(a.length) == 0 => Some(x :: tail)
       case _ => None
     }
  }
  */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, None) => None
      case (_, None) => None
      case (None, _) => None
      case (Some(x), Some(y)) => Some(f(x, y))
    }
  }

  /*
  Here's an explicit recursive version:
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
  Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
  unfortunate consequence of Scala using subtyping to encode algebraic data types.
  */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  //ADAM
/*
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val start = Some(List(): List[A]): Option[List[A]]
    def merger(e: Option[A], z: Option[List[A]]): Option[List[A]] = e.flatMap(ev => z.map(l => ev :: l))

    a.foldRight(start)(merger)
  }
*/

  //KONRAD
/*  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil=> Some(Nil)
    case x :: xs => x.flatMap( xval => sequence(xs).map( xsval => xval :: xsval))
  }*/


  //PATRYK
 /* def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(acc: Option[List[A]], as: List[Option[A]]) : Option[List[A]] = {
      as match {
        case Nil => acc
        case None :: xs => None
        case x :: xs => go(map2(acc, x)((acc, x) => acc ++ List(x)), xs)
      }
    }
    go(Some(List()), a)
  }*/



  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val bs: List[B] = a.foldLeft(Nil: List[B])((x, y) => x ::: f(y).map(yy => List(yy)).getOrElse(Nil: List[B]))

    bs match {
      case Nil => None
      case x :: tail if (x :: tail).lengthCompare(a.length) == 0 => Some(x :: tail)
      case _ => None
    }

    //    Option.sequence(a.map(f))
  }

/* def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
   val start: Either[E, List[B]] = Right(List())
   as.foldRight(start)((e: A, z: Either[E, List[B]]) => f(e).flatMap(eo => z.map(l => eo :: l)))
 }


  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x :: xs =>
      for {
        fx <- f(x)
        el <- traverse(xs)(f)
      } yield fx :: el
  }*/
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
