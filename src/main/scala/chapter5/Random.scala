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
    val intRng: (Int, RNG) = rNG.nextInt
    val doubleRng: (Double, RNG) = double(intRng._2)
    ((intRng._1, doubleRng._1), doubleRng._2)
  }

  def doubleInt(rNG: RNG): ((Double, Int), RNG) = {
    val intRng: (Int, RNG) = rNG.nextInt
    val doubleRng: (Double, RNG) = double(intRng._2)
    ((doubleRng._1, intRng._1), doubleRng._2)
  }

  def double3(rNG: RNG): ((Double, Double, Double), RNG) = {
    val intRng: (Int, RNG) = rNG.nextInt
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
}
