package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class TimeoutFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

    override def isCancelled: Boolean = ???

    override def isDone: Boolean = ???

    override def get(timeout: Long, unit: TimeUnit): A = ???
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  //[KF] Ex. 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val aFuture: Future[A] = a(es)
      val bFuture: Future[B] = b(es)
      UnitFuture(f(aFuture.get(5000, TimeUnit.MILLISECONDS), bFuture.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  //Ex 7.3
  ???

  //Ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  //Ex 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = (es: ExecutorService) => unit(ps.foldRight(Nil: List[A])((pA: Par[A], acc: List[A]) => List(pA(es).get()) ::: acc))(es)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //Ex 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    val list: List[Par[A]] = as.map(lazyUnit)

    def filterEl(l: List[Par[A]]): List[Par[A]] = list match {
      case Nil => Nil
      case t :: h if f(t) => List(t) ::: filterEl(h)
      case _ :: h => filterEl(h)
    }

    sequence(filterEl(list))
  }
}
