package chapter5

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimepleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRNG = SimepleRNG(newSeed)
    val n: Int = (newSeed >>> 16).toInt
    (n, newRNG)
  }
}

object Random {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // EX 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, x) => (0, x)
      case (x, y) => (Math.abs(x), y)
    }
  }

  // EX 6.2
  def double(rng: RNG): (Double, RNG) = {
    val result: (Int, RNG) = nonNegativeInt(rng)
    (result._1.toDouble / Int.MaxValue.toDouble, result._2)
  }

  // EX 6.3
  def intDouble(rNG: RNG): ((Int, Double), RNG) = {
    val intRng: (Int, RNG) = nonNegativeInt(rNG)
    val doubleRng: (Double, RNG) = double(intRng._2)
    ((intRng._1, doubleRng._1), doubleRng._2)
  }

  def doubleInt(rNG: RNG): ((Double, Int), RNG) = {
    val intRng: (Int, RNG) = nonNegativeInt(rNG)
    val doubleRng: (Double, RNG) = double(intRng._2)
    ((doubleRng._1, intRng._1), doubleRng._2)
  }

  def double3(rNG: RNG): ((Double, Double, Double), RNG) = {
    val intRng: (Int, RNG) = nonNegativeInt(rNG)
    val double1Rng: (Double, RNG) = double(intRng._2)
    val double2Rng: (Double, RNG) = double(intRng._2)
    val double3Rng: (Double, RNG) = double(intRng._2)
    ((double1Rng._1, double2Rng._1, double3Rng._1), double3Rng._2)
  }

  // EX 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = n match {
      case 0 => (acc, rng)
      case _ =>
        val tuple: (Int, RNG) = rng.nextInt
        go(n - 1, tuple._2, acc ::: List(tuple._1))
    }

    go(count, rng, Nil: List[Int])
  }

  // PARAGRAPH 6.4
  val int: Rand[Int] = _.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // EX 6.5
  def doubleWithMap: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  // EX 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val raResult = ra(rng)
    val rbResult = rb(raResult._2)
    (f(raResult._1, rbResult._1), rbResult._2)
  }

  def intDoubleWithMap(rNG: RNG): Rand[(Int, Double)] = map2(nonNegativeInt, double)((_, _))

  // EX 6.7
  // type Rand[+A] = RNG => (A, RNG)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.reverse.foldLeft((Nil: List[A], rng))((b, a) => {
      val newVal = a(b._2)
      (b._1 ::: List(newVal._1), newVal._2)
    })
  }

  //EX 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val tuple = f(rng)
    g(tuple._1)(tuple._2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => rng => {
      val mod: Int = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng)
      else
        nonNegativeLessThan(n)(rng)
    })
  }

  //EX 6.9
  def mapFL[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => rng => (f(x), rng))

  def map2FL[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
}
