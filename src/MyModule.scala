import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  //exercise 2.1
  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n <= 2) {
      1
    } else fibonacci(n - 1) + fibonacci(n - 2)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}