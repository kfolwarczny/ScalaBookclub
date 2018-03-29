package chapter5

import chapter5.Random.Rand
import org.scalatest.{FlatSpec, Matchers}

class RandomTest extends FlatSpec with Matchers {

  //TODO: how to test?
  it should "positive" in {
    val res: (Int, RNG) = Random.nonNegativeInt(SimepleRNG(100))

    println(res._1)

    assert(res._1 == 38474890)
    assert(res._1 > 0)
  }

  it should "be between <0, 1)" in {
    val res: (Double, RNG) = Random.double(SimepleRNG(100))

    println(res._1)

    assert(res._1 == 0.017916266814766577)
    assert(res._1 >= 0)
    assert(res._1 < 1)
  }

  it should "generate list of randoms" in {

    val res: (List[Int], RNG) = Random.ints(5)(SimepleRNG(1000))

    res._1 should be {
      List(384748900, -134141098, 2135606878, 352450840, 288539203)
    }
  }

  it should "be between <0, 1) with doubleWithMap" in {
    val res: (Double, RNG) = Random.doubleWithMap(SimepleRNG(100))

    println(res._1)

    assert(res._1 == 0.017916266814766577)
    assert(res._1 >= 0)
    assert(res._1 < 1)
  }

  it should "generate list of randoms with sequence" in {
    val list: List[Rand[Int]] = List(rng => rng.nextInt, rng => rng.nextInt, rng => rng.nextInt, rng => rng.nextInt, rng => rng.nextInt)
    val value: Rand[List[Int]] = Random.sequence(list)

    val res = value(SimepleRNG(100))
    res._1 should be {
      List(38474890, 419891633, 374484099, 288555289, 1438213179)
    }
  }

  it should "nonNegativeLessThan" in {

    val value: Rand[Int] = Random.nonNegativeLessThanPZI(10)
    val res = value(SimepleRNG(50))

    res._1 should be {
      5
    }
  }

}
