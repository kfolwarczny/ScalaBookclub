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

    println("\n Ex 3.8")
    println(FPList.foldRight(FPList(1, 2, 3), Nil: FPList[Int])(Cons(_, _)))

    println("\n Ex 3.9")
    println("Original: " + l)
    println("Expected length: 0, actual: " + FPList.length(Nil))
    println("Expected length: 5, actual: " + FPList.length(l))
    println("Expected length: 4, actual: " + FPList.length(FPList(2, 4, 6, 1)))

    println("\n Ex 3.10")
    println("Original: " + l)
    println("Expected sum to 15, actual: " + FPList.foldLeft(l, 0)(_ + _))
    println("Expected sum to 15, actual: " + FPList.foldLeft2(l, 0)(_ + _))

    println("\n Ex 3.11")
    println("Original: " + l)
    println("Expected sum to 15, actual: " + FPList.sum(l))
    println("Expected sum to 15, actual: " + FPList.sumFL(l))
    println("Expected product to 120, actual: " + FPList.product(FPList(1.0, 2.0, 3.0, 4.0, 5.0)))
    println("Expected product to 120, actual: " + FPList.productFL(l))
    println("Expected length: 4, actual: " + FPList.lengthFL(FPList(2, 4, 6, 1)))

    println("\n Ex 3.11")
    println("Original: " + l)
    println("Reverse: " + FPList.reverse(l))
  }
}
