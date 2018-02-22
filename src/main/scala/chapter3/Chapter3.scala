package chapter3

object Chapter3 extends App {

  override def main(args: Array[String]): Unit = {
    val l = FPList(1, 2, 3, 4, 5)
    println("Ex 3.1: 3")

    println("\nEx 3.2 Tail")
    println("Original: " + l)
    println("Should return Nil: " + FPList.tail(Nil))
    println("Should return tail: " + FPList.tail(l))

    println("\nEx 3.3")
    println("Original: " + l)
    println("Should return Nil: " + FPList.setHead(Nil, 2))
    println("Should return set new head: " + FPList.setHead(l, 22))

    println("\n Ex 3.4")
    println("Original: " + l)
    println("Should drop first 2: " + FPList.drop(l, 2))
    println("Should drop all and return Nil: " + FPList.drop(l, 15))
    println("Should return Nil for Nil: " + FPList.drop(Nil, 2))

    println("\n Ex 3.5")
    println("Original: " + l)
    println("Should drop first 3: " + FPList.dropWhile(l, (a: Int) => a <= 3))

    println("\n Ex 3.6")
    println("Original: " + l)
    println("Should be [1,2,3,4]: " + FPList.init(l))
    println("Should be Nil: " + FPList.init(Nil))
    println("Should be Nil: " + FPList.init(Cons(1, Nil)))
  }
}
