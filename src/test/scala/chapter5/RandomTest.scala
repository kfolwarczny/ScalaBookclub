package chapter5

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

}
