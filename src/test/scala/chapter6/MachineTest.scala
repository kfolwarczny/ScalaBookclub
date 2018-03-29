package chapter6

import org.scalatest.{FlatSpec, Matchers}

class MachineTest extends FlatSpec with Matchers {
  "Machine" should "1" in {
    Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn,Coin, Turn))
      .run(Machine(locked = true, 5, 10)) should be ((14, 1), Machine(locked = true, 14, 1))
  }
}
