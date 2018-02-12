object Chapter2 extends App {

  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {

    def go(n: Int, acc: Int, prev: Int): Int = {

      if (n == 0) acc
      else go(n - 1, acc + prev, acc)
    }

    go(n, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def go(n: Int, acc: Boolean): Boolean = {

      if (n + 1 >= as.length) acc
      else go(n + 1, ordered(as(n), as(n + 1)))

    }

    go(0, acc = false)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  override def main(args: Array[String]): Unit = {

    println("Factorial")
    println(factorial(1))
    println(factorial(5))

    println("Fibonacci")
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))

    println("isSorted")
    val sorted = Array(1, 2, 3, 4)
    val notSorted = Array(1, 4, 3, 0)
    println(isSorted[Int](sorted, (a, b) => a <= b))
    println(isSorted[Int](notSorted, (a, b) => a <= b))
  }
}

