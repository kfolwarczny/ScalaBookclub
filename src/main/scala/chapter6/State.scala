package chapter6

// EX 6.10
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(v => {
    val (a, s) = run(v)
    (f(a), s)
  })

  def mapFL[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))


  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(st => {
    val (a, s) = run(st)
    val (b, newS) = sb.run(s)
    (f(a, b), newS)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(st => {
    val (a, s) = run(st)
    f(a).run(s)
  })
}


object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = State(st => {
    sas.reverse.foldLeft((Nil: List[A], st))((b, s) => {
      val (a, newS) = s.run(b._2)
      (b._1 ::: List(a), newS)
    })
  })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    get.flatMap(s => set(f(s)).map(_ => ()))
}