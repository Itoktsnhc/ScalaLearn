import scala.annotation.tailrec

object Exercise {
  //exercise 2.1
  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n <= 2) {
      1
    } else fibonacci(n - 1) + fibonacci(n - 2)
  }

  //exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }

    loop(1)
  }

  def main(args: Array[String]): Unit =
    println(isSorted(Array(1, 2, 2, 4, 5), (i: Int, j: Int) => {
      i <= j
    }))
}
