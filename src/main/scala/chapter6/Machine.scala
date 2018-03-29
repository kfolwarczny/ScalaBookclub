package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/*
- Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
- Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
- Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
- A machine that’s out of candy ignores all inputs.”
 */

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

