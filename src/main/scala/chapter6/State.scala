package chapter6

import chapter5.Random.Rand

case class State[S,+A](run: S => (A,S)) {
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val tuple = f(rng)
    g(tuple._1)(tuple._2)
  }
}

